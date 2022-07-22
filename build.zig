const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();


    const main_tests = b.addTest("src/kule.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    

    const cli = cliStep(b, mode);

    const run_cmd = cli.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the CLI");
    run_step.dependOn(&run_cmd.step);

    const server = cliStep(b, mode);
    server.setOutputDir("./dev");

    // const install_dev = std.build.InstallFileStep.init(b, cli.install_name)

    const dev_step = b.step("install-dev", "Build the CLI and install it in ./dev/");
    dev_step.dependOn(&server.step);
}

fn cliStep(b: *std.build.Builder, mode: std.builtin.Mode) *std.build.LibExeObjStep {
    const cli = b.addExecutable("kule", "src/cli.zig");
    cli.setBuildMode(mode);
    cli.install();
    return cli;
}