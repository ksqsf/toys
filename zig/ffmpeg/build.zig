const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("ffmpeg", "main.zig");
    exe.linkLibC();
    exe.linkSystemLibrary("libavcodec");
    exe.linkSystemLibrary("libavformat");
    exe.linkSystemLibrary("libavfilter");
    exe.linkSystemLibrary("libavdevice");
    exe.linkSystemLibrary("swresample");
    exe.linkSystemLibrary("swscale");
    exe.linkSystemLibrary("avutil");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
