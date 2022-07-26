const std = @import("std");

const bld = std.build;

pub fn build(b: *bld.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const all_tests_step = b.step("test", "perform all tests");

    (Exe{
        .name = "kule",
        .root = "src/cli.zig",
        .step_name = "cli",
        .description = "kule cli",
    }).add(b, mode, all_tests_step);

    (Exe{
        .name = "kule-server",
        .root = "src/server.zig",
        .step_name = "server",
        .description = "kule language server",
    }).add(b, mode, all_tests_step);

}

const Exe = struct {
    name: []const u8,
    root: []const u8,
    step_name: []const u8,
    description: []const u8,

    fn add(comptime e: Exe, b: *bld.Builder, mode: std.builtin.Mode, all_tests: *bld.Step) void {
        const exe = b.addExecutable(e.name, e.root);
        exe.setBuildMode(mode);
        const run = exe.run();
        if (b.args) |args| {
            run.addArgs(args);
        }
        const install = bld.InstallArtifactStep.create(b, exe);
        b.default_step.dependOn(&install.step);
        const install_step = b.step(e.step_name, "install " ++ e.description);
        install_step.dependOn(&install.step);
        const run_step = b.step("run-" ++ e.step_name, "run " ++ e.description);
        run_step.dependOn(&run.step);
        const tests = b.addTest(e.root);
        const test_step = b.step("test-" ++ e.step_name, "perform " ++ e.description ++ " tests");
        test_step.dependOn(&tests.step);
        all_tests.dependOn(&tests.step);
    }

};