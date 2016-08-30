export HOME=/root
export LC_ALL=C
cd /
apt-get -t jessie --no-install-recommends --yes install git wget ca-certificates && 
apt-get -t jessie --yes install gcc make && git clone https://github.com/linux-nvme/nvme-cli.git && 
cd nvme-cli && 
make && make install && 
cd .. && 
rm -rf nvme-cli && 
wget https://github.com/Drive-Trust-Alliance/exec/blob/master/sedutil_LINUX.tgz?raw=true && 
mv sedutil_LINUX.tgz?raw=true sedutil_LINUX.tgz && 
tar zxvf sedutil_LINUX.tgz && 
cp ./sedutil/Release_x86_64/GNU-Linux/sedutil-cli /usr/local/sbin && 
rm -rf ./sedutil && 
rm -f ./sedutil_LINUX.tgz
apt-get autoremove --yes git gcc make wget && 
apt-get autoremove --yes
