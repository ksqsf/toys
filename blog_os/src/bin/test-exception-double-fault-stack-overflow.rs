#![feature(panic_implementation)]
#![feature(abi_x86_interrupt)]
#![no_std]
#![cfg_attr(not(test), no_main)]
#![cfg_attr(test, allow(dead_code, unused_macros, unused_imports))]

#[macro_use]
extern crate blog_os;
#[macro_use]
extern crate lazy_static;
extern crate x86_64;

use core::panic::PanicInfo;
use x86_64::structures::idt::{InterruptDescriptorTable, ExceptionStackFrame};
use blog_os::exit_qemu;

#[cfg(not(test))]
#[no_mangle]
pub extern "C" fn _start() -> ! {
    blog_os::gdt::init();
    init_idt();

    #[allow(unconditional_recursion)]
    fn stack_overflow() {
        stack_overflow();
    }
    stack_overflow();

    serial_println!("failed");
    serial_println!("No exception occured");

    unsafe { exit_qemu(); }
    loop {}
}

#[cfg(not(test))]
#[panic_implementation]
#[no_mangle]
pub fn panic(info: &PanicInfo) -> ! {
    println!("{}", info);
    loop {}
}

lazy_static! {
    static ref IDT: InterruptDescriptorTable = {
        let mut idt = InterruptDescriptorTable::new();
        unsafe {
            idt.double_fault.set_handler_fn(double_fault_handler)
                .set_stack_index(blog_os::gdt::DOUBLE_FAULT_IST_INDEX);
        }
        idt
    };
}

pub fn init_idt() {
    IDT.load();
}

extern "x86-interrupt" fn double_fault_handler(
    _stack_frame: &mut ExceptionStackFrame,
    _error_code: u64)
{
    serial_println!("ok");
    unsafe { exit_qemu(); }
    loop {}
}
