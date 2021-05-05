#!/bin/bash -e
#
#  bootstrap_dev.sh
#
#  Install marelle for the current user only.
#

function has_exec() {
  [ ! -z "$(which $1)" ]
}

function missing_exec() {
  [ -z "$(which $1)" ]
}

is_apt_updated=0
function apt_update() {
  if [ $is_apt_updated -eq 0 ]; then
    sudo apt-get update
    is_apt_updated=1
  fi
}

is_brew_updated=0
function brew_update() {
  if [ $is_brew_updated -eq 0 ]; then
    brew update
    is_brew_updated=1
  fi
}

function install_git() {
  echo 'Trying to install git'
  case $(uname -s) in
    Darwin)
      if has_exec brew; then
        brew_update
        brew install git
      else
        bail "Please install Homebrew and retry"
      fi
      ;;
    Linux)
      if has_exec apt-get; then
        apt_update
        sudo apt-get install -y git
      elif has_exec yum; then
        # XXX yum update?
        sudo yum install git
      else
        bail "Unknown linux variant"
      fi
      ;;
    FreeBSD)
      if has_exec pkg; then
        sudo pkg install -y git
      else
        bail "Old FreeBSD version without pkgng"
      fi
      ;;
    *)
      bail "Unknown operating system $(uname -s)"
      ;;
  esac
}

function install_prolog() {
  echo 'Trying to install prolog'
  case $(uname -s) in
    Darwin)
      if has_exec brew; then
        brew_update
        brew install swi-prolog
      else
        bail "Please install Homebrew and retry"
      fi
      ;;
    Linux)
      if has_exec apt-get; then
        apt_update
        sudo apt-get install -y swi-prolog-nox
      elif has_exec yum; then
        sudo yum install swi-prolog
      else
        bail "Unknown linux variant"
      fi
      ;;
    FreeBSD)
      if has_exec pkg; then
        sudo pkg install -y swi-pl
      else
        bail "Old FreeBSD version without pkgng"
      fi
      ;;
    *)
      bail "Unknown operating system $(uname -s)"
      ;;
  esac
}

function bail()
{
  echo "$1 -- bailing"
  exit 1
}

function check_in_path() {
  echo $PATH | tr ':' '\n' | grep -x -c "$1";
}


function checkout_marelle() {
  echo 'Trying to check out marelle'
  mkdir -p ~/.local/bin
  git clone https://github.com/larsyencken/marelle ~/.local/marelle
  cat >~/.local/bin/marelle <<EOF
#!/bin/sh
exec swipl -q -t main -s ~/.local/marelle/marelle.pl "\$@"
EOF
  chmod a+x ~/.local/bin/marelle
  if [ ! -d ~/.local/marelle -o ! -x ~/.local/bin/marelle ]; then
    bail "Ran into a problem checking out marelle"
  fi
}

function put_marelle_in_path() {
  echo 'Checking if marelle is in PATH'
  if [ -f ~/.bash_profile ]; then
    echo 'export PATH=~/.local/bin:$PATH' >>~/.bash_profile
    source ~/.bash_profile
  elif [ -f ~/.profile ]; then
    echo 'export PATH=~/.local/bin:$PATH' >>~/.profile
    source ~/.profile
  fi
  if missing_exec marelle; then
    bail "Couldn't set up marelle in PATH. Add ~/.local/bin to your PATH in your shell's rc."
  fi
}

function main() {
  echo 'BOOTSTRAPPING MARELLE'

  if missing_exec git; then
    install_git
  fi
  echo 'Git: OK'

  if missing_exec swipl; then
    install_prolog
  fi
  echo 'Prolog: OK'

  if [ ! -d ~/.local/marelle ]; then
    checkout_marelle
  fi
  echo 'Marelle: OK'

  hash -r
  if missing_exec marelle; then
    put_marelle_in_path
  fi
  echo 'Marelle in PATH: OK'
  echo 'DONE'
}

main
