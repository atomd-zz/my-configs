
n=python$version

echo "== This script will install Distribute, pip, and virtualenv"
echo "   in your home directory for Python $version."

DISTRIBUTE_TEMP=$(mktemp -p "${TMPDIR:-.}" -d distribute-XXX) || {
    echo "Could not create temporary working directory"
    exit 1
}
pushd $DISTRIBUTE_TEMP

echo "== Creating ~/.pydistutils.cfg..."
if [ ! -e ~/.pydistutils.cfg ]; then
    cat >~/.pydistutils.cfg <<CONFIG
[install]
install_lib = ~/.local/lib/python$version/site-packages
install_scripts = ~/.local/bin

CONFIG
fi

mkdir -p ~/.local/lib/python$version/site-packages
mkdir -p ~/.local/bin

echo "== Downloading Distribute installer..."
curl -O http://python-distribute.org/distribute_setup.py

echo "== Installing Distribute..."
echo "!! If you get a permissions error in the \"After install bootstrap\""
echo "   section, ignore it. It will still work."
echo

$python distribute_setup.py

echo
echo "== Installing pip..."
$HOME/.local/bin/easy_install-$version pip

echo
echo "== Installing virtualenv..."
$HOME/.local/bin/pip-$version install virtualenv

echo
echo "== Cleaning up..."
popd
rm -r $DISTRIBUTE_TEMP
echo "== Done! To use your new Python tools, add to your \$PATH:"
echo "   $HOME/.local/bin"
