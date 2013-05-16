# Check if the currently checked out revision is pointed to by a tag.
# If it is, print the tag. If not, print the SHA-1.
rev=`git describe --all | sed -n '1,/tags/s/^tags\/\(\)/\1/p'`
if [ -z "$rev" ]; then
    rev=`git rev-parse HEAD`
fi
echo "$rev\c"
