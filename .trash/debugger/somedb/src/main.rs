extern crate libc;
extern crate nix;
#[macro_use]
extern crate log;
extern crate env_logger;

use std::ffi::CString;

use nix::unistd::*;
use nix::sys::ptrace;
use nix::sys::wait::*;

fn main() -> Result<(), String> {
    env_logger::init();

    let mut args = std::env::args();
    if args.len() < 2 {
        return Err("Expected a program name".into())
    }

    match fork() {
        Ok(ForkResult::Parent { child }) => run_debugger2(child),
        Ok(ForkResult::Child) => run_target(&args.nth(1).unwrap()),
        Err(_) => Err("fork failed".into())
    }
}

fn run_target(progname: &str) -> Result<(), String> {
    info!("target started. will run '{}'", progname);

    // Allow tracing me
    if let Err(_) = ptrace::traceme() {
        return Err("traceme failed".into());
    }

    // Replace image
    let progname = CString::new(progname).unwrap();
    match execv(&progname, &[progname.clone()]) {
        Ok(_) => Ok(()),
        Err(e) => Err(format!("execv failed with {:?}", e))
    }
}

#[allow(dead_code)]
fn run_debugger(child: Pid) -> Result<(), String> {
    info!("debugger started, tracing pid {}", child);

    let mut icnt: u64 = 0;

    while let Ok(WaitStatus::Stopped(_, _)) = wait() {
        icnt += 1;
        // get PC and instr
        let pc = get_rip(child);
        let instr = peek_text(child, pc);
        info!("icnt={}, PC={:016x}, instr={:016x}", icnt, pc, instr);
        // single step
        //
        // Note that the order of these two steps can't be reversed!
        // GETREGS requires the process to be ptrace-stopped, or it
        // fails with ESRCH
        if let Err(e) = ptrace::step(child, None) {
            return Err(format!("step failed with {:?}", e));
        }
    }

    println!("child executed {} instructions", icnt);
    Ok(())
}

/// This function only works with bp
#[cfg(target_arch="x86_64")]
fn run_debugger2(child: Pid) -> Result<(), String> {
    // wait for child to stop
    match wait() {
        Ok(WaitStatus::Stopped(_, _)) => (),
        Ok(_) => return Err(format!("unknown wait result")),
        Err(e) => return Err(format!("wait failed with {:?}", e))
    }

    // get PC
    let pc = get_rip(child);
    info!("child started. rip={:016x}", pc);

    // check (quad-)word
    let addr = pc + 0x16;
    let word0 = peek_text(child, addr);
    info!("original word at {:016x}: {:016x}", addr, word0);

    // poke (quad-)word
    let word = word0 & 0xffffffffffffff00 | 0xcc;
    poke_text(child, addr, word);
    // check again
    let word = peek_text(child, addr);
    info!("after setting the breakpoint: {:016x}", word);
    // resume
    let _ = ptrace::cont(child, None);

    // wait again
    match wait() {
        Ok(WaitStatus::Stopped(_, sig)) => info!("child got a signal {:?}", sig),
        Ok(_) => info!("unknown state"),
        Err(e) => info!("wait failed with {:?}", e)
    }

    // see where we are now
    let pc = get_rip(child);
    info!("child stopped at {:016x}", pc);

    // rewind and restore instruction
    poke_text(child, addr, word0);
    set_rip(child, pc-1);

    // check again
    let word = peek_text(child, addr);
    info!("after clearing the breakpoint: {:016x}", word);

    // continue
    let _ = ptrace::cont(child, None);
    info!("continuing");

    // wait for child to exit
    let _ = wait();
    info!("child exited successfully");

    Ok(())
}

#[cfg(target_arch="x86_64")]
fn get_rip(child: Pid) -> u64 {
    unsafe {
        let mut regs: libc::user_regs_struct = std::mem::uninitialized();
        libc::ptrace(libc::PTRACE_GETREGS,
                     child,
                     0,
                     &mut regs);
        regs.rip
    }
}

#[cfg(target_arch="x86_64")]
fn set_rip(child: Pid, pc: u64) {
    unsafe {
        let mut regs: libc::user_regs_struct = std::mem::uninitialized();
        libc::ptrace(libc::PTRACE_GETREGS, child, 0, &mut regs);
        regs.rip = pc;
        libc::ptrace(libc::PTRACE_SETREGS, child, 0, &regs);
    }
}

#[cfg(target_arch="x86_64")]
fn peek_text(child: Pid, rip: u64) -> u64 {
    let instr;
    unsafe {
        instr = libc::ptrace(libc::PTRACE_PEEKTEXT,
                             child,
                             rip,
                             0);
    }
    instr as u64
}

#[cfg(target_arch="x86_64")]
fn poke_text(child: Pid, pc: u64, word: u64) {
    unsafe {
        libc::ptrace(libc::PTRACE_POKETEXT,
                     child, pc, word);
    }
}
