# Bash doesn't do macros, but how about a function or two?
setlink() {
    if [ -f $1 ]; then
	ln -s $1 .
    else
	echo ASDF definition file $1 appears to be missing
    fi
}

add() {
    if [ -d ../$1 ]; then
	echo Adding $1
	setlink ../$1/$1.asd
    else
	echo Subproject $1 appears to be missing
    fi
}


# Projects that follow the general pattern...
PROJECTS="core glue"
# ..and projects that don't.
FILES="../core/ocore.asd ../glue/aglue.asd"


for project in $PROJECTS; do
    add $project
done

for file in $FILES; do
    setlink $file
done
