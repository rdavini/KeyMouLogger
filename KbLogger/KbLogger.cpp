#include <ntddk.h>
#include <ntddkbd.h>
#include <ntddmou.h>

#pragma warning(disable:4996)

extern "C"
extern POBJECT_TYPE* IoDriverObjectType;

typedef bool BOOL;

#define KBD_DEVICE 0
#define MOU_DEVICE 1

typedef struct {
	PDEVICE_OBJECT lowerKbdDev;
	LIST_ENTRY queueHead;
	KSPIN_LOCK queueLock;
	HANDLE pFileHandle;
	PETHREAD pThread;
	KSEMAPHORE semQueue;
	BOOL threadTerminated;
	ULONG deviceType;
} DEVICE_EXTENSION, *PDEVICE_EXTENSION;

typedef struct {
	KEYBOARD_INPUT_DATA key;
	LIST_ENTRY entry;
} KEY_DATA;

typedef struct {
	MOUSE_INPUT_DATA mouse_input;
	LIST_ENTRY entry;
} MOU_DATA;

typedef struct {
	CHAR separator;
	ULONG delay;
} SEPARATOR;

extern "C"
NTSTATUS
NTAPI
ObReferenceObjectByName(IN PUNICODE_STRING ObjectPath,
	IN ULONG Attributes,
	IN PACCESS_STATE PassedAccessState,
	IN ACCESS_MASK DesiredAccess,
	IN POBJECT_TYPE ObjectType,
	IN KPROCESSOR_MODE AccessMode,
	IN OUT PVOID ParseContext,
	OUT PVOID* ObjectPtr);

void DriverUnloadRoutine(PDRIVER_OBJECT);
NTSTATUS DriverDispatchRoutine(PDEVICE_OBJECT, PIRP);
NTSTATUS DispatchRead(PDEVICE_OBJECT DevObj, PIRP irp);
NTSTATUS ReadComplete(PDEVICE_OBJECT DeviceObject, PIRP Irp, PVOID Context);
void SleepKernel(ULONG milliseconds);
VOID w2f(PVOID StartContext);
NTSTATUS CreateDevice(PDRIVER_OBJECT DrvObj, PDEVICE_OBJECT* devObj, unsigned int DeviceType);
NTSTATUS InitDevice(PDRIVER_OBJECT DrvObj, unsigned int DeviceType);
VOID printIrpContent(SEPARATOR sep, PMOUSE_INPUT_DATA irpContent);
VOID printIrpContent(SEPARATOR sep, PKEYBOARD_INPUT_DATA irpContent);
ULONG getTimeDiff(PLARGE_INTEGER startTime, PLARGE_INTEGER endTime);

BOOL fW2f;
ULONG key_pending;
LARGE_INTEGER tLastInput;

extern "C"
NTSTATUS DriverEntry(PDRIVER_OBJECT DriverObject, PUNICODE_STRING RegistryPath) {
	UNREFERENCED_PARAMETER(RegistryPath);
	NTSTATUS status = STATUS_SUCCESS;
	fW2f = false;
	key_pending = 0;

	for (int i = 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++) {
		DriverObject->MajorFunction[i] = DriverDispatchRoutine;
	}

	DriverObject->MajorFunction[IRP_MJ_READ] = DispatchRead;
	DriverObject->DriverUnload = DriverUnloadRoutine;
	
	KeQuerySystemTimePrecise(&tLastInput);

	InitDevice(DriverObject, KBD_DEVICE);
	InitDevice(DriverObject, MOU_DEVICE);

	return status;
}

NTSTATUS CreateDevice(PDRIVER_OBJECT DrvObj, PDEVICE_OBJECT * devObj, unsigned int DeviceType) {
	ULONG fDeviceType = (DeviceType == KBD_DEVICE ? FILE_DEVICE_KEYBOARD : FILE_DEVICE_MOUSE);
	NTSTATUS status = IoCreateDevice(DrvObj, sizeof(DEVICE_EXTENSION), NULL, fDeviceType, 0, true, devObj);
	if (!NT_SUCCESS(status)) {
		KdPrint(("Failed to create device\n"));
		return status;
	}
	(*devObj)->Flags |= DO_BUFFERED_IO;
	(*devObj)->Flags &= ~DO_DEVICE_INITIALIZING;

	return status;
}

NTSTATUS InitDevice(PDRIVER_OBJECT DrvObj, unsigned int DeviceType) {
	NTSTATUS status = STATUS_SUCCESS;
	UNICODE_STRING fName = RTL_CONSTANT_STRING(L"\\??\\C:\\Users\\rdavini\\Documents\\kbdMouLogger.txt");
	PDEVICE_EXTENSION pDevExt = NULL;
	PDEVICE_OBJECT devObj;

	//refactor use only one variable to hold MOU\KBD path
	UNICODE_STRING mouPath = RTL_CONSTANT_STRING(L"\\Driver\\MouClass");
	UNICODE_STRING kbdPath = RTL_CONSTANT_STRING(L"\\Driver\\KbdClass");		
		
	PDRIVER_OBJECT mouDriver;

	if (!DeviceType) {
		status = ObReferenceObjectByName(&kbdPath, 0, NULL, (ACCESS_MASK)0, *IoDriverObjectType, KernelMode, NULL, (PVOID*)&mouDriver);
	}
	else {
		status = ObReferenceObjectByName(&mouPath, 0, NULL, (ACCESS_MASK)0, *IoDriverObjectType, KernelMode, NULL, (PVOID*)&mouDriver);
	}

	PDEVICE_OBJECT currentDev = mouDriver->DeviceObject;

	while (currentDev) {
		status = CreateDevice(DrvObj, &devObj, DeviceType);

		pDevExt = (PDEVICE_EXTENSION)devObj->DeviceExtension;
		RtlZeroMemory(pDevExt, sizeof(DEVICE_EXTENSION));

		pDevExt->lowerKbdDev = IoAttachDeviceToDeviceStack(devObj, currentDev);

		if (!NT_SUCCESS(status)) {
			KdPrint(("Failed to attach device\n"));
			return status;
		}

		if (pDevExt != NULL) {
			pDevExt->deviceType = DeviceType;
			KdPrint(("deviceType: %d\n", DeviceType));
			pDevExt->threadTerminated = false;
			InitializeListHead(&pDevExt->queueHead);
			KeInitializeSpinLock(&pDevExt->queueLock);
			KeInitializeSemaphore(&pDevExt->semQueue, 0, MAXLONG);
		}

		OBJECT_ATTRIBUTES fObjAttr;
		IO_STATUS_BLOCK fIoStatusBlock;
		InitializeObjectAttributes(&fObjAttr, &fName, OBJ_CASE_INSENSITIVE, NULL, NULL);

		status = ZwOpenFile(&pDevExt->pFileHandle, FILE_APPEND_DATA, &fObjAttr, &fIoStatusBlock, FILE_SHARE_WRITE, FILE_SYNCHRONOUS_IO_NONALERT);
		if (!NT_SUCCESS(status)) {
			KdPrint(("Failed to open file\n"));
			return status;
		}

		HANDLE hThread;
		status = PsCreateSystemThread(&hThread, (ACCESS_MASK)0, NULL, (HANDLE)0, NULL, w2f, pDevExt);
		if (!NT_SUCCESS(status)) {
			KdPrint(("Failed to create thread\n"));
			return status;
		}

		status = ObReferenceObjectByHandle(hThread, THREAD_ALL_ACCESS, NULL, KernelMode, (PVOID*)&pDevExt->pThread, NULL);
		if (!NT_SUCCESS(status)) {
			KdPrint(("Failed to open pointer to thread obj\n"));
			return status;
		}

		ZwClose(hThread);

		currentDev = currentDev->NextDevice;
	}

	return status;
}

ULONG getTimeDiff(PLARGE_INTEGER startTime, PLARGE_INTEGER endTime) {
	return (endTime->LowPart - startTime->LowPart) / 10000;
}

VOID w2f(PVOID StartContext) {
	UNREFERENCED_PARAMETER(StartContext);
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)StartContext;
	IO_STATUS_BLOCK fIoStatusBlock;
	NTSTATUS status = STATUS_SUCCESS;
	LARGE_INTEGER tInput;
	SEPARATOR sep;
	ULONG timeDiff;

	while (true) {		
		KeWaitForSingleObject(&pDevExt->semQueue, Executive, KernelMode, FALSE, NULL);

		if (pDevExt->threadTerminated) {
			KdPrint(("Terminating thread %d\n", pDevExt->deviceType));
			break;
		}
		KeQuerySystemTimePrecise(&tInput);

		// Print the difference in ticks
		timeDiff = getTimeDiff(&tLastInput, &tInput);
		KdPrint(("Time Difference (millesec): %ul\n", timeDiff));

		//CalculateTimeDifference(tLastInput, tInput);
		tLastInput = tInput;		
		
		sep.delay = timeDiff;
		sep.separator = pDevExt->deviceType == KBD_DEVICE ? '0' : '1';

		PLIST_ENTRY entry2 = ExInterlockedRemoveHeadList(&pDevExt->queueHead, &pDevExt->queueLock);
		ZwWriteFile(pDevExt->pFileHandle, NULL, NULL, NULL, &fIoStatusBlock, &sep, sizeof(SEPARATOR), 0, 0);
		if (pDevExt->deviceType == KBD_DEVICE) {
			KEY_DATA* kData = CONTAINING_RECORD(entry2, KEY_DATA, entry);
			KdPrint(("W2f keyboard\n"));
			printIrpContent(sep, &kData->key);
			ZwWriteFile(pDevExt->pFileHandle, NULL, NULL, NULL, &fIoStatusBlock, &kData->key, sizeof(KEYBOARD_INPUT_DATA), 0, 0);
		}
		else if (pDevExt->deviceType == MOU_DEVICE) {
			sep.separator = '1';
			MOU_DATA * mData = CONTAINING_RECORD(entry2, MOU_DATA, entry);
			KdPrint(("W2f mouse\n"));
			printIrpContent(sep, &mData->mouse_input);
			status = ZwWriteFile(pDevExt->pFileHandle, NULL, NULL, NULL, &fIoStatusBlock, &mData->mouse_input, sizeof(MOUSE_INPUT_DATA), 0, 0);
			KdPrint(("status: %x\n", status));
		}
	}
	PsTerminateSystemThread(STATUS_SUCCESS);
}

VOID printIrpContent(SEPARATOR sep, PMOUSE_INPUT_DATA irpContent) {
	KdPrint(("Sep: sep: %c, delay: %ul \t MouInput unitid:%x \t flags:%x \t rawButtons:%x \t buttons:%d \t Button buttonData: %d \t lastX: %x \t lastY: %x \t extraInformation: %x", sep.separator, sep.delay, irpContent->UnitId, irpContent->Flags, irpContent->RawButtons, irpContent->Buttons, irpContent->ButtonData, irpContent->LastX, irpContent->LastY, irpContent->ExtraInformation));
}

VOID printIrpContent(SEPARATOR sep, PKEYBOARD_INPUT_DATA irpContent) {
	KdPrint(("Sep: sep: %c, delay: %ul \tKbdInput unitid:%x \t flags:%x \t makeCode:%x \t reserved: %x \t extraInformation: %x", sep.separator, sep.delay, irpContent->UnitId , irpContent->Flags, irpContent->MakeCode , irpContent->Reserved, irpContent->ExtraInformation));
}

void SleepKernel(ULONG milliseconds)
{
	LARGE_INTEGER interval;

	// Convert milliseconds to 100-nanosecond intervals
	interval.QuadPart = -(LONGLONG)milliseconds * 10000;

	// Delay execution of the current thread
	KeDelayExecutionThread(KernelMode, FALSE, &interval);
}

void DriverUnloadRoutine(PDRIVER_OBJECT DriverObject) {
	PDEVICE_OBJECT dev = DriverObject->DeviceObject;
	while (dev) {
		PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)dev->DeviceExtension;

		pDevExt->threadTerminated = true;
		IoDetachDevice(pDevExt->lowerKbdDev);
		dev = dev->NextDevice;
	}
	
	while (key_pending) {
		KdPrint(("pending_key: %d \n", key_pending));
		SleepKernel(1000);		
	}

	dev = DriverObject->DeviceObject;
	while(dev){
		PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)dev->DeviceExtension;
		ZwClose(pDevExt->pFileHandle);
		ObDereferenceObject(pDevExt->pThread);

		IoDeleteDevice(dev);
		dev = dev->NextDevice;
	}
}

NTSTATUS DriverDispatchRoutine(PDEVICE_OBJECT DevObj, PIRP irp) {
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)DevObj->DeviceExtension;

	IoSkipCurrentIrpStackLocation(irp);
	return IoCallDriver(pDevExt->lowerKbdDev, irp);
}

NTSTATUS DispatchRead(PDEVICE_OBJECT DevObj, PIRP irp) {
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)DevObj->DeviceExtension;
	
	IoCopyCurrentIrpStackLocationToNext(irp);
	
	IoSetCompletionRoutine(irp, ReadComplete, pDevExt, true, true, true);
	
	key_pending++;

	return IoCallDriver(pDevExt->lowerKbdDev, irp);
}

NTSTATUS ReadComplete(PDEVICE_OBJECT DeviceObject, PIRP Irp, PVOID Context) {
	UNREFERENCED_PARAMETER(DeviceObject);
	UNREFERENCED_PARAMETER(Context);

	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)Context;

	KdPrint(("Read Complete\n"));
	KdPrint(("device: %d\n", pDevExt->deviceType));
	if (Irp->IoStatus.Status == STATUS_SUCCESS) {
		KdPrint(("key_pending: %d\n", key_pending));
		if (!pDevExt->deviceType) {
			PKEYBOARD_INPUT_DATA input = (PKEYBOARD_INPUT_DATA)Irp->AssociatedIrp.SystemBuffer;

			if (input->MakeCode == 0x003D && input->Flags) {
				fW2f = !fW2f;
				KdPrint((fW2f ? "on\n" : "off\n"));
			}

			KdPrint(("input: %d \n", input->MakeCode));

			KEY_DATA* entry = (KEY_DATA*)ExAllocatePoolWithTag(NonPagedPool, sizeof(KEY_DATA), 'ktag');
			RtlZeroMemory(entry, sizeof(KEY_DATA));

			entry->key = *input;

			if (fW2f && input->MakeCode != 0x003D) {
				ExInterlockedInsertTailList(&pDevExt->queueHead, &entry->entry, &pDevExt->queueLock);
				KeReleaseSemaphore(&pDevExt->semQueue, 0, 1, FALSE);
			}
			ExFreePoolWithTag(entry, 'ktag');
		}
		else {
			PMOUSE_INPUT_DATA input = (PMOUSE_INPUT_DATA)Irp->AssociatedIrp.SystemBuffer;
			MOU_DATA* entry = (MOU_DATA*)ExAllocatePoolWithTag(NonPagedPool, sizeof(MOU_DATA), 'mtag');
			RtlZeroMemory(entry, sizeof(MOU_DATA));
			
			entry->mouse_input = *input;
			
			KdPrint(("\nMouInput unitid:%x \t flags:%x \t rawButtons:%x \t buttons:%d \t Button buttonData: %d \t lastX: %x \t lastY: %x \t extraInformation: %x", entry->mouse_input.UnitId, entry->mouse_input.Flags, entry->mouse_input.RawButtons, entry->mouse_input.Buttons, entry->mouse_input.ButtonData, entry->mouse_input.LastX, entry->mouse_input.LastY, entry->mouse_input.ExtraInformation));
			
			switch (entry->mouse_input.ButtonFlags) {
				case MOUSE_LEFT_BUTTON_DOWN:
					KdPrint(("Button buttonFlags: MOUSE_LEFT_BUTTON_DOWN \t"));
					break;
				case MOUSE_LEFT_BUTTON_UP:
					KdPrint(("Button buttonFlags: MOUSE_LEFT_BUTTON_UP \t"));
					break;
				case MOUSE_RIGHT_BUTTON_DOWN:
					KdPrint(("Button buttonFlags: MOUSE_RIGHT_BUTTON_DOWN \t"));
					break;
				case MOUSE_RIGHT_BUTTON_UP:
					KdPrint(("Button buttonFlags: MOUSE_RIGHT_BUTTON_UP \t"));
					break;
				case MOUSE_MIDDLE_BUTTON_DOWN:
					KdPrint(("Button buttonFlags: MOUSE_MIDDLE_BUTTON_DOWN \t"));
					break;
				case MOUSE_MIDDLE_BUTTON_UP:
					KdPrint(("Button buttonFlags: MOUSE_MIDDLE_BUTTON_UP \t"));
					break;
				case MOUSE_BUTTON_4_DOWN:
					KdPrint(("Button buttonFlags: MOUSE_BUTTON_4_DOWN \t"));
					break;
				case MOUSE_BUTTON_4_UP:
					KdPrint(("Button buttonFlags: MOUSE_BUTTON_4_UP \t"));
					break;
				case MOUSE_BUTTON_5_DOWN:
					KdPrint(("Button buttonFlags: MOUSE_BUTTON_5_DOWN \t"));
					break;
				case MOUSE_BUTTON_5_UP:
					KdPrint(("Button buttonFlags: MOUSE_BUTTON_5_UP \t"));
					break;
				case MOUSE_WHEEL:
					KdPrint(("Button buttonFlags: MOUSE_WHEEL \t"));
					break;
				case MOUSE_HWHEEL:
					KdPrint(("Button buttonFlags: MOUSE_HWHEEL \t"));
					break;
				default:
					KdPrint(("Button buttonFlags: %d not treated \t", entry->mouse_input.ButtonFlags));
					break;
			}
			
			if (fW2f) {
				ExInterlockedInsertTailList(&pDevExt->queueHead, &entry->entry, &pDevExt->queueLock);
				KeReleaseSemaphore(&pDevExt->semQueue, 0, 1, FALSE);
			}
			ExFreePoolWithTag(entry, 'mtag');
		}
	}
	
	if (Irp->PendingReturned) {
		IoMarkIrpPending(Irp);
	}

	key_pending--;
	
	return Irp->IoStatus.Status;
}