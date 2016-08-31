export HOME=/root
export LC_ALL=C
cd /
apt-get -t jessie --yes install gcc make && cd nvme-cli && 
make && make install && 
cd .. && 
rm -rf nvme-cli && 
cp /sedutil/Release_x86_64/GNU-Linux/sedutil-cli /usr/local/sbin && 
rm -rf ./sedutil
apt-get autoremove --yes gcc make