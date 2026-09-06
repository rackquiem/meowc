(* Output naming and linker flags follow the object file format, not the
   operating system: the same triple can name a platform whose format decides
   whether a shared library carries a lib prefix, needs an import library, or
   has any notion of a soname at all. *)
type t = Elf | Macho | Coff | Wasm

let of_platform = function
  | "darwin" -> Macho
  | "windows" -> Coff
  | "wasm" -> Wasm
  | _ -> Elf

let name = function Elf -> "elf" | Macho -> "macho" | Coff -> "coff" | Wasm -> "wasm"
let exe_ext = function Coff -> ".exe" | Wasm -> ".wasm" | Elf | Macho -> ""

(* Archives are written by ar and read back through -l, so they are named the
   same way everywhere, including for the gnu ABI on Windows. *)
let static_name n = "lib" ^ n ^ ".a"

(* COFF keeps the plain name and links through a separate import library, while
   the other formats carry the prefix in the shared object name itself. *)
let shared_name fmt n =
  match fmt with
  | Elf -> "lib" ^ n ^ ".so"
  | Macho -> "lib" ^ n ^ ".dylib"
  | Coff -> n ^ ".dll"
  | Wasm -> "lib" ^ n ^ ".wasm"

let import_name fmt n = match fmt with Coff -> Some ("lib" ^ n ^ ".dll.a") | _ -> None

(* COFF and wasm are position independent by construction, and their drivers
   warn about a flag that cannot mean anything to them. *)
let pic_flags = function Elf | Macho -> [ "-fPIC" ] | Coff | Wasm -> []
let shared_flags = function Macho -> [ "-dynamiclib" ] | Elf | Coff | Wasm -> [ "-shared" ]

let soname_flags fmt s =
  match fmt with
  | Elf -> [ "-Wl,-soname," ^ s ]
  | Macho -> [ "-Wl,-install_name," ^ s ]
  | Coff | Wasm -> []

let implib_flags fmt path =
  match fmt with Coff -> [ "-Wl,--out-implib," ^ path ] | Elf | Macho | Wasm -> []
