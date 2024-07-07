# Environment

We are using [Nix flakes](https://nixos.wiki/wiki/Flakes) to set up whole development environment.

When you have configured Nix. Enter into [nix shell](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-develop) and then run:
```bash
nix run
```

Nix will setup a database with development required schemas for you.

After `dev-database` Health status is `Ready`, you can start to work with database dependent code parts.

# Profiling

```bash
nix run .#profiling
```

will start profiling process.

You should see `profiler.eventlog.html` in the project \
directory after the profiling process has completed successfully
