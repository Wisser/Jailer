rm -rf ~/jailer_win/
cp -r /media/hda1/jailer_win ~/
rm -r /media/hda1/jailer_win
cp -r . /media/hda1/jailer_win
cd /media/hda1/jailer_win
find . -iname ".svn" -exec rm -r '{}' \;

