module Janet = Janet

exception Janet_error = Janet_errors.Janet_error

let init = Janet.Vm.init
let deinit = Janet.Vm.deinit
let with_janet = Janet.Vm.with_vm
let with_janet_env = Janet.with_janet_env
let check_signal = Janet.check_signal
let dostring = Janet.dostring
let dostring_exn = Janet.dostring_exn
let dobytes = Janet.dobytes
let dobytes_exn = Janet.dobytes_exn
let mcall = Janet.mcall
let mcall_exn = Janet.mcall_exn

module JanetCfun = Cfun.JanetCfun

let register_cfun = Cfun.register_raw
