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
        Ok(ForkResult::Parent { child }) => run_debugger(child),
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
        if let Err(e) = ptrace::step(child, None) {
            return Err(format!("step failed with {:?}", e));
        }
    }

    println!("child executed {} instructions", icnt);
    Ok(())
}

#[cfg(target_arch="x86_64")]
fn get_rip(child: Pid) -> u64 {
    unsafe {
        let mut regs: libc::user_regs_struct = std::mem::zeroed();
        let res = libc::ptrace(libc::PTRACE_GETREGS,
                               child,
                               0,
                               &mut regs);
        if res == -1 {
            eprintln!("can't get regs {:?}", nix::Error::last());
        }
        regs.rip
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
