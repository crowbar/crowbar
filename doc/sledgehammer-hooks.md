This file documents the hooks available in the Sledgehammer image that
can be used to customize how Sledgehammer transitions through states.

The primary Sledgehammer control script (control.sh) will search for
-pre and -post hooks for every Crowbar state transition it knows how
to go through. The hooks can be general (all nodes transitioning
through the state will run them) or specific to one machine. This
functionality is intended to be used to facilitate automated testing
that must happen outside of the chef-client runs.  Hooks will run in
the following order:

1: Machine-specific -pre hooks,
2: General -pre hooks,
3: chef-client,
4: General -post hooks,
5: Machine-specific -post hooks.

All hooks will be run in alphabetic order, and non-executable hooks
will be skipped.  All hooks to be run should be staged in the /updates
NFS share on the admin node in the following directories:

 * /updates/$nodename/$state-pre
 * /updates/$state-pre
 * /updates/$state-post
 * /updates/$nodename/$state-post

$state = the Crowbar state that Sledgehammer is transitioning through.
         discovering, discovered, update, etc.

$nodename = the name of the node according to Chef.

To make writing hooks simpler, certian functions in the control.sh script
have been split into /updates/control_lib.sh, and control.sh has been
refactored to account for these changes.  If your hooks are written in
bash, they can source /updates/control_lib.sh to pull in the following
functions:

* get_state <node>
  get_state will pull some information about <node> from the
  chef-server, and store it in the local environment.  Specifically,
  it will set the following environment variables:
  BMC_ROUTER = the gateway IP address of the BMC network, if any.
  BMC_ADDRESS = the IP address of the BMC for <node>, if any.
  BMC_NETMASK = the netmask in dotted quad format of the BMC network,
                if any.
  CROWBAR_STATE = the current Crowbar state that the node is in.
  HOSTNAME = the hostname of the system according to the Chef server.
  ALLOCATED = whether or not <node> has been allocated

* post_state <node> <crowbar_state>
  post_state will attempt to transition <node> to <state>.  If
  successful, it will then update the same environment data that
  get_state does.

* reboot_system
  This will cleanly reboot the node.  You should use it instead of
  reboot to ensure that all the logs are flushed and that the NFS
  shares get umounted.

* wait_for_allocated <node>
  This function will spin until <node> has been allocated by Crowbar.

* wait_for_crowbar_state <node> <state>
  If <state> is not passed, this function will wait until <node>
  transitions to a state other than the one it is currently at.  If
  <state> is passed, this function will wait until <node> transitions
  into <state>

* hook_has_run
  If the current hook has already run for the current -pre or -post
  state, hook_has_run will return 0.  Otherwise, return 1.  This
  function works by creating state tracking files in /install-logs/.
  To facilitate this function, control.sh exports the following
  environment variables to the hooks:
  * HOOKNAME = the name of the hook without any path components.
  * HOOKSTATE = the -pre or -post state that the hook is running in.

Hook Exit Status

All hooks must exit successfully (with a 0 return status), otherwise
the system will reboot into the debug state.
