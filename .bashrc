# ~/.bashrc: executed by bash(1) for non-login interactive shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# --- Set DISPLAY for X11 (VcXsrv) ---
export DISPLAY=$(ip route show | grep default | awk '{print $3}'):0.0

# --- Start D-Bus session if none exists ---
if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval $(dbus-launch --sh-syntax)
fi

# --- Aliases for Emacs ---
alias emacst="DISPLAY= emacs -nw"   # terminal mode
alias emacsg="emacs"                # GUI mode, requires DISPLAY

# --- Optional PATH modifications ---
export PATH="$HOME/bin:$PATH"

# --- End of WSL BashRC ---
