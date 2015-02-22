module PInvokeHelper 

    open System
    open System.Threading
    open System.IO
    open System.Runtime.InteropServices
    open Microsoft.FSharp.NativeInterop

    type HANDLE = nativeint
    type ADDR = nativeint

    type PageProtection =
            | NoAccess = 0x01
            | Readonly = 0x02
            | ReadWrite = 0x04
            | WriteCopy = 0x08
            | Execute = 0x10
            | ExecuteRead = 0x20
            | ExecuteReadWrite = 0x40
            | ExecuteWriteCopy = 0x80
            | Guard = 0x100
            | NoCache = 0x200
            | WriteCombine = 0x400

    [<DllImport("kernel32", SetLastError = true)>]
    extern bool CloseHandle(HANDLE handler)

    [<DllImport("kernel32", SetLastError = true, CharSet = CharSet.Auto)>]
    extern HANDLE CreateFile(string lpFileName,
                             int dwDesiredAccess,
                             int dwShareMode,
                             HANDLE lpSecurityAttributes,
                             int dwCreationDisposition,
                             int dwFlagsAndAttributes,
                             HANDLE hTemplateFile)

//    [<DllImport("kernel32", SetLastError = true, CharSet = CharSet.Auto)>]
//    extern HANDLE CreateFileMapping(HANDLE hFile,
//                                    HANDLE lpAttributes,
//                                    int flProtect,
//                                    int dwMaximumSizeLow,
//                                    int dwMaximumSizeHigh,
//                                    string lpName)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern IntPtr CreateFileMapping(IntPtr hFile,
            IntPtr lpFileMappingAttributes, PageProtection flProtect,
            uint32 dwMaximumSizeHigh,
            uint32 dwMaximumSizeLow, string lpName);


    [<DllImport("kernel32", SetLastError = true, CharSet = CharSet.Auto)>]
    extern ADDR MapViewOfFile(HANDLE hFileMappingObject,
                              int dwDesiredAccess,
                              int dwFileOffsetLow,
                              int dwNumBytesToMap,
                              HANDLE dwNumberOfBytesToMap)

    [<DllImport("kernel32", SetLastError = true, CharSet = CharSet.Auto)>]
    extern HANDLE OpenFileMapping(int dwDesiredAccess,
                                  bool bInheritHandle,
                                  string lpName)

    [<DllImport("kernel32", SetLastError = true)>]
    extern bool UnmapViewOfFile(ADDR lpBaseAddress)

    let INVALID_HANDLE = new IntPtr(-1)
    let MAP_READ = 0x0004
    let GENERIC_READ = 0x80000000
    let NULL_HANDLE = IntPtr.Zero
    let FILE_SHARE_NONE = 0x0000
    let FILE_SHARE_READ = 0x0001
    let FILE_SHARE_WRITE = 0x0002
    let FILE_SHARE_READ_WRITE = 0x0003
    let CREATE_ALWAYS = 0x0002
    let OPEN_EXISTING = 0x0003
    let OPEN_ALWAYS = 0x0004
    let READONLY = 0x0000000