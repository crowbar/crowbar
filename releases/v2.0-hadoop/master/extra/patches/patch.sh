#!/bin/bash
# This applies some common patches that Crowbar needs for Chef to
# function the way we expect.
patches=(
    'ohai/mixin/command.rb' # OHAI-330
    'chef/mixin/command/unix.rb' # CHEF-2916
    'chef-server-ap*/app/controllers/data_item.rb' # CHEF-2005
    'chef/run_list.rb' # Always return run_lists sanely.
    'ohai-0.6.6/lib/ohai/plugins/linux/platform.rb'
    'chef*/provider/package/rubygems.rb' # Make chef honor custom gem options.
)

for p in "${patches[@]}"; do
    f="$(find /usr -path "*${p}")"
    [[ $f && -f $f ]] || {
        echo "Cannot find file to patch for $p, will skip."
        continue
    }
    for patch in "$PWD/${f##*/}"*".patch"; do
        [[ -f $patch ]] || {
            echo "No patch file for $f, will skip."
            continue 2
        }
        [[ -f $f.crowbar-backup ]] && continue 2
        cp "$f" "$f.crowbar-backup"
        (cd "${f%/*}"; patch <"$patch") && continue 2
        mv "$f.crowbar-backup" "$f"
        echo "Patch $patch did not cleanly apply to $f, skipping it."
    done
done
