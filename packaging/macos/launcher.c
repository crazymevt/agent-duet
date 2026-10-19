/* The macOS app's executable, Contents/MacOS/Agent Duet: runs
 * Contents/MacOS/sbcl (SBCL's runtime) on Contents/Resources/agent-duet.core,
 * passing its own arguments on. It replaces itself (execv), so the app keeps
 * this process, and with it its identity in the Dock. From Cadre's launcher;
 * scripts/make-app.sh compiles it. */
#include <errno.h>
#include <libgen.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  char path[PATH_MAX], real[PATH_MAX];
  uint32_t size = sizeof path;
  if (_NSGetExecutablePath(path, &size) != 0 || !realpath(path, real)) {
    fprintf(stderr, "Agent Duet: can't find where the app is\n");
    return 1;
  }
  char *macos = dirname(real); /* Contents/MacOS */
  char runtime[PATH_MAX], core[PATH_MAX];
  snprintf(runtime, sizeof runtime, "%s/sbcl", macos);
  snprintf(core, sizeof core, "%s/../Resources/agent-duet.core", macos);

  char *fixed[] = {runtime, "--core", core, "--dynamic-space-size", "2048",
                   "--noinform", "--end-runtime-options"};
  int nfixed = sizeof fixed / sizeof fixed[0];
  char **args = calloc(nfixed + argc, sizeof(char *));
  if (!args) return 1;
  memcpy(args, fixed, sizeof fixed);
  for (int i = 1; i < argc; i++) args[nfixed + i - 1] = argv[i];
  execv(runtime, args);
  fprintf(stderr, "Agent Duet: can't run %s: %s\n", runtime, strerror(errno));
  return 1;
}
