#include <ntddk.h>
#include <ntddkbd.h>
#include <ntddmou.h>

#pragma warning(disable:4996)

extern "C"
extern POBJECT_TYPE* IoDriverObjectType;

typedef bool BOOL;

#define KBD_DEVICE '0'
#define MOU_DEVICE '1'
#define START_ACTION '2'
#define END_ACTION '3'

typedef struct {
	PDEVICE_OBJECT lowerDev;
	LIST_ENTRY queueHead;
	KSPIN_LOCK queueLock;
	HANDLE pFileHandle;
	PETHREAD pThread;
	KSEMAPHORE semQueue;
	BOOL threadTerminated;
	CHAR deviceType;
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
	LARGE_INTEGER time;
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
NTSTATUS CreateDevice(PDRIVER_OBJECT DrvObj, CHAR DeviceType, PHANDLE hFile);
VOID printIrpContent(SEPARATOR sep, PMOUSE_INPUT_DATA irpContent);
VOID printIrpContent(SEPARATOR sep, PKEYBOARD_INPUT_DATA irpContent);

BOOL fW2f;
ULONG key_pending;

extern "C"
NTSTATUS DriverEntry(PDRIVER_OBJECT DriverObject, PUNICODE_STRING RegistryPath) {
	UNREFERENCED_PARAMETER(RegistryPath);
	NTSTATUS status = STATUS_SUCCESS;
	IO_STATUS_BLOCK fIoStatusBlock;
	OBJECT_ATTRIBUTES fObjAttr;
	UNICODE_STRING fName = RTL_CONSTANT_STRING(L"\\??\\C:\\Users\\rdavini\\Documents\\ScreenRec\\kbdMouLog.txt");
	HANDLE hFile;

	fW2f = false;
	key_pending = 0;

	for (int i = 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++) {
		DriverObject->MajorFunction[i] = DriverDispatchRoutine;
	}

	DriverObject->MajorFunction[IRP_MJ_READ] = DispatchRead;
	DriverObject->DriverUnload = DriverUnloadRoutine;

	InitializeObjectAttributes(&fObjAttr, &fName, OBJ_CASE_INSENSITIVE, NULL, NULL);

	status = ZwCreateFile(&hFile, FILE_WRITE_DATA, &fObjAttr, &fIoStatusBlock, NULL, FILE_ATTRIBUTE_NORMAL, FILE_SHARE_WRITE, FILE_SUPERSEDE, FILE_SYNCHRONOUS_IO_NONALERT, NULL, 0);
	if (!NT_SUCCESS(status)) {
		KdPrint(("Failed to open file\n"));
		return status;
	}

	CreateDevice(DriverObject, KBD_DEVICE, &hFile);
	CreateDevice(DriverObject, MOU_DEVICE, &hFile);

	return status;
}

NTSTATUS CreateDevice(PDRIVER_OBJECT DrvObj, CHAR DeviceType, PHANDLE hFile) {
	NTSTATUS status = STATUS_SUCCESS;
	PDEVICE_EXTENSION pDevExt = NULL;
	PDEVICE_OBJECT devObj;
	UNICODE_STRING drvPath;

	DeviceType == KBD_DEVICE ? drvPath = RTL_CONSTANT_STRING(L"\\Driver\\KbdClass") : drvPath = RTL_CONSTANT_STRING(L"\\Driver\\MouClass");

	PDRIVER_OBJECT driver;

	status = ObReferenceObjectByName(&drvPath, 0, NULL, (ACCESS_MASK)0, *IoDriverObjectType, KernelMode, NULL, (PVOID*)&driver);
	if (!NT_SUCCESS(status)) {
		DbgPrint("Failed to get pointer to driver obj");
	}

	PDEVICE_OBJECT currentDev = driver->DeviceObject;
	ULONG fDeviceType = (DeviceType == KBD_DEVICE ? FILE_DEVICE_KEYBOARD : FILE_DEVICE_MOUSE);

	while (currentDev) {
		DbgPrint("Device type: %s current dev %x\n",DeviceType == KBD_DEVICE ? "KEYBOARD" : "MOUSE", currentDev);
		status = IoCreateDevice(DrvObj, sizeof(DEVICE_EXTENSION), NULL, fDeviceType, 0, true, &devObj);
		if (!NT_SUCCESS(status)) {
			KdPrint(("Failed to create device\n"));
			return status;
		}
		devObj->Flags |= DO_BUFFERED_IO;
		devObj->Flags &= ~DO_DEVICE_INITIALIZING;

		pDevExt = (PDEVICE_EXTENSION)devObj->DeviceExtension;
		RtlZeroMemory(pDevExt, sizeof(DEVICE_EXTENSION));

		pDevExt->lowerDev = IoAttachDeviceToDeviceStack(devObj, currentDev);

		if (!NT_SUCCESS(status)) {
			KdPrint(("Failed to attach device\n"));
			return status;
		}

		if (pDevExt != NULL) {
			pDevExt->deviceType = DeviceType;
			KdPrint(("deviceType: %d\n", DeviceType));
			pDevExt->threadTerminated = false;
			pDevExt->pFileHandle = *hFile;
			InitializeListHead(&pDevExt->queueHead);
			KeInitializeSpinLock(&pDevExt->queueLock);
			KeInitializeSemaphore(&pDevExt->semQueue, 0, MAXLONG);
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

	ObDereferenceObject(driver);

	return status;
}

VOID w2f(PVOID StartContext) {
	UNREFERENCED_PARAMETER(StartContext);
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)StartContext;
	SEPARATOR sep = { 0 };
	PLIST_ENTRY entry2 = NULL;
	IO_STATUS_BLOCK fIoStatusBlock;

	DbgPrint("Started w2f");
		
	sep.separator = pDevExt->deviceType;
	while (!pDevExt->threadTerminated) {
		KeWaitForSingleObject(&pDevExt->semQueue, Executive, KernelMode, FALSE, NULL);

		if (pDevExt->threadTerminated) {
			KdPrint(("Terminating thread %d\n", pDevExt->deviceType));
			break;
		}

		entry2 = ExInterlockedRemoveHeadList(&pDevExt->queueHead, &pDevExt->queueLock);

		if (entry2) {
			KeQuerySystemTimePrecise(&sep.time);
			if (pDevExt->deviceType == KBD_DEVICE) {
				KEY_DATA* kData = CONTAINING_RECORD(entry2, KEY_DATA, entry);
				
				//the user needs to press f3 to write inputs to file
				if (kData->key.MakeCode == 0x003D && kData->key.Flags) {
					fW2f = !fW2f;

					sep.separator = fW2f ? START_ACTION : END_ACTION;
					DbgPrint("start writing file");
					ZwWriteFile(pDevExt->pFileHandle, NULL, NULL, NULL, &fIoStatusBlock, &sep, sizeof(SEPARATOR), 0, 0);
					sep.separator = pDevExt->deviceType;
					
					DbgPrint(fW2f ? "on\n" : "off\n");
				}

				if (kData->key.MakeCode != 0x003D && fW2f) {
					printIrpContent(sep, &kData->key);
					DbgPrint("writing kbd");
					ZwWriteFile(pDevExt->pFileHandle, NULL, NULL, NULL, &fIoStatusBlock, &sep, sizeof(SEPARATOR), 0, 0);
					ZwWriteFile(pDevExt->pFileHandle, NULL, NULL, NULL, &fIoStatusBlock, &kData->key, sizeof(KEYBOARD_INPUT_DATA), 0, 0);
				}
			}
			else if (pDevExt->deviceType == MOU_DEVICE) {
				MOU_DATA * mData = CONTAINING_RECORD(entry2, MOU_DATA, entry);
				
				if (fW2f) {
					DbgPrint("writing mouse");
					printIrpContent(sep, &mData->mouse_input);
					ZwWriteFile(pDevExt->pFileHandle, NULL, NULL, NULL, &fIoStatusBlock, &sep, sizeof(SEPARATOR), 0, 0);
					ZwWriteFile(pDevExt->pFileHandle, NULL, NULL, NULL, &fIoStatusBlock, &mData->mouse_input, sizeof(MOUSE_INPUT_DATA), 0, 0);
				}
			}
		}
	}
	PsTerminateSystemThread(STATUS_SUCCESS);
}

VOID printIrpContent(SEPARATOR sep, PMOUSE_INPUT_DATA irpContent) {
	DbgPrint("sep: %c, time: %ul \t MouInput: \n\t { \n\t\t unitid:%x \n\t\t flags:%x \n\t\t rawButtons:%x \n\t\t buttons:%d \n\t\t Button flags: %d \n\t\t buttonData: %d \n\t\t lastX: %x \n\t\t lastY: %x \n\t\t extraInformation: %x\n", sep.separator, sep.time, irpContent->UnitId, irpContent->Flags, irpContent->RawButtons, irpContent->Buttons, irpContent->ButtonFlags, irpContent->ButtonData, irpContent->LastX, irpContent->LastY, irpContent->ExtraInformation);
}

VOID printIrpContent(SEPARATOR sep, PKEYBOARD_INPUT_DATA irpContent) {
	DbgPrint("sep: %c, time: %ul \t KbdInput: \n\t { \n\t\t unitid:%x \n\t\t flags:%x \n\t\t makeCode:%x \n\t\t reserved: %x \n\t\t extraInformation: %x \n", sep.separator, sep.time, irpContent->UnitId , irpContent->Flags, irpContent->MakeCode , irpContent->Reserved, irpContent->ExtraInformation);
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
	PDEVICE_EXTENSION pDevExt;
	while (dev) {
		pDevExt = (PDEVICE_EXTENSION)dev->DeviceExtension;
		pDevExt->threadTerminated = true;
		IoDetachDevice(pDevExt->lowerDev);
		dev = dev->NextDevice;
	}
	
	while (key_pending) {
		KdPrint(("pending_key: %d \n", key_pending));
		SleepKernel(1000);		
	}

	dev = DriverObject->DeviceObject;
	while(dev){
		pDevExt = (PDEVICE_EXTENSION)dev->DeviceExtension;
		ZwClose(pDevExt->pFileHandle);
		ObDereferenceObject(pDevExt->pThread);

		IoDeleteDevice(dev);
		dev = dev->NextDevice;
	}
}

NTSTATUS DriverDispatchRoutine(PDEVICE_OBJECT DevObj, PIRP irp) {
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)DevObj->DeviceExtension;

	IoSkipCurrentIrpStackLocation(irp);


	return IoCallDriver(pDevExt->lowerDev, irp);
}

NTSTATUS DispatchRead(PDEVICE_OBJECT DevObj, PIRP irp) {
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)DevObj->DeviceExtension;
	
	IoCopyCurrentIrpStackLocationToNext(irp);
	
	IoSetCompletionRoutine(irp, ReadComplete, pDevExt, true, true, true);
	
	key_pending++;

	return IoCallDriver(pDevExt->lowerDev, irp);
}

NTSTATUS ReadComplete(PDEVICE_OBJECT DeviceObject, PIRP Irp, PVOID Context) {
	UNREFERENCED_PARAMETER(DeviceObject);
	UNREFERENCED_PARAMETER(Context);

	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)Context;

	if (Irp->IoStatus.Status == STATUS_SUCCESS) {
		if (pDevExt->deviceType == KBD_DEVICE) {
			PKEYBOARD_INPUT_DATA input = (PKEYBOARD_INPUT_DATA)Irp->AssociatedIrp.SystemBuffer;
			KEY_DATA* entry = (KEY_DATA*)ExAllocatePoolWithTag(NonPagedPool, sizeof(KEY_DATA), 'ktag');
			RtlZeroMemory(entry, sizeof(KEY_DATA));

			entry->key = *input;

			ExInterlockedInsertTailList(&pDevExt->queueHead, &entry->entry, &pDevExt->queueLock);
			KeReleaseSemaphore(&pDevExt->semQueue, 0, 1, FALSE);
		}
		else {
			PMOUSE_INPUT_DATA input = (PMOUSE_INPUT_DATA)Irp->AssociatedIrp.SystemBuffer;
			MOU_DATA* entry = (MOU_DATA*)ExAllocatePoolWithTag(NonPagedPool, sizeof(MOU_DATA), 'mtag');
			RtlZeroMemory(entry, sizeof(MOU_DATA));
			
			entry->mouse_input = *input;
			
			ExInterlockedInsertTailList(&pDevExt->queueHead, &entry->entry, &pDevExt->queueLock);
			KeReleaseSemaphore(&pDevExt->semQueue, 0, 1, FALSE);			
		}
	}
	
	if (Irp->PendingReturned) {
		IoMarkIrpPending(Irp);
	}

	key_pending--;
	
	return Irp->IoStatus.Status;
}