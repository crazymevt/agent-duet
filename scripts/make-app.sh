#!/bin/bash
# Put the core scripts/build-app.lisp saved into a macOS app bundle
# (make app runs it). Adapted from Cadre's scripts/make-app.sh. Writes
# build/Agent Duet.app:
#
#   Contents/MacOS/Agent Duet   the launcher (packaging/macos/launcher.c)
#   Contents/MacOS/sbcl         SBCL's runtime, the one that saved the core
#   Contents/Resources/agent-duet.core
#
# GTK, libadwaita and VTE come from Homebrew on the Mac running it; claude
# and codex from the login shell's PATH (or their usual places). Signed with
# CODESIGN_IDENTITY, or ad hoc ("-") if it isn't set.
set -euo pipefail
cd "$(dirname "$0")/.."

sbcl=${SBCL:-sbcl}
identity=${CODESIGN_IDENTITY:--}
core=build/app/agent-duet.core
app="build/Agent Duet.app"
version=$(sed -n 's/.*:version "\([^"]*\)".*/\1/p' agent-integration.asd | head -1)
[ -f "$core" ] || { echo "No $core: run scripts/build-app.lisp first (make app does)" >&2; exit 1; }

rm -rf "$app"
contents="$app/Contents"
mkdir -p "$contents/MacOS" "$contents/Resources"
cp "$core" "$contents/Resources/agent-duet.core"
cp LICENSE "$contents/Resources/LICENSE"

# The launcher, and the runtime that saved the core (they must match).
cc -O2 -Wall -mmacosx-version-min=12.0 -o "$contents/MacOS/Agent Duet" packaging/macos/launcher.c
runtime=$("$sbcl" --noinform --non-interactive --no-userinit --no-sysinit --eval '(princ sb-ext:*runtime-pathname*)')
cp "$runtime" "$contents/MacOS/sbcl"
chmod u+w "$contents/MacOS/sbcl"

# The icon, from packaging/AgentDuet.svg.
iconset=build/app/AgentDuet.iconset
rm -rf "$iconset"; mkdir -p "$iconset"
for size in 16 32 128 256 512; do
  rsvg-convert -w $size -h $size packaging/AgentDuet.svg -o "$iconset/icon_${size}x${size}.png"
  rsvg-convert -w $((size * 2)) -h $((size * 2)) packaging/AgentDuet.svg -o "$iconset/icon_${size}x${size}@2x.png"
done
iconutil -c icns "$iconset" -o "$contents/Resources/AgentDuet.icns"

cat > "$contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleExecutable</key><string>Agent Duet</string>
  <key>CFBundleIdentifier</key><string>io.github.crazymevt.AgentDuet</string>
  <key>CFBundleName</key><string>Agent Duet</string>
  <key>CFBundleDisplayName</key><string>Agent Duet</string>
  <key>CFBundleIconFile</key><string>AgentDuet</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleShortVersionString</key><string>$version</string>
  <key>CFBundleVersion</key><string>$version</string>
  <key>LSApplicationCategoryType</key><string>public.app-category.developer-tools</string>
  <key>LSMinimumSystemVersion</key><string>12.0</string>
  <key>NSHighResolutionCapable</key><true/>
  <key>NSHumanReadableCopyright</key><string>Copyright © 2026 Jessie Hughart. MIT License.</string>
</dict>
</plist>
PLIST

# Sign the runtime (the process the launcher becomes), then the app.
sign=(codesign --force --sign "$identity" --options runtime)
[ "$identity" != "-" ] && sign+=(--timestamp)
"${sign[@]}" --entitlements packaging/macos/AgentDuet.entitlements "$contents/MacOS/sbcl"
"${sign[@]}" --entitlements packaging/macos/AgentDuet.entitlements "$app"
codesign --verify --deep --strict "$app"

echo "Wrote $app ($(du -sh "$app" | cut -f1), signed by ${identity/#-/ad hoc})"
