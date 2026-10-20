#!/usr/bin/env python3
"""Print one version's changelog body; fail rather than publish empty notes."""
import re
import sys
from pathlib import Path


def release_notes(changelog, version):
    sections = re.split(r"^## +", changelog, flags=re.MULTILINE)
    for section in sections[1:]:
        heading, _, body = section.partition("\n")
        if heading.split()[0] == version:
            if body.strip():
                return body.strip() + "\n"
            break
    raise ValueError(f"No nonempty changelog section for {version}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit("Usage: release-notes.py VERSION")
    try:
        print(release_notes(Path("CHANGELOG.md").read_text(), sys.argv[1]), end="")
    except ValueError as error:
        sys.exit(str(error))
