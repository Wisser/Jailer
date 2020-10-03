dir=`pwd`
echo $dir
rm -rf result
mkdir result
for filename in *.zip; do 
	echo $filename
	rm -rf ~/_
	mkdir ~/_
	unzip $filename -d ~/_ > /dev/null
	chmod a+x ~/_/jailer/*.sh
	chmod a+x ~/_/jailer/*.bat
	chmod a+x ~/_/jailer/*.exe
	cd ~/_
	zip -r $dir/result/$filename *
	cd $dir
	pwd
done
