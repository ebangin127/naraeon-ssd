cd ~
sudo apt-get update
sudo apt-get --yes git
git clone git@github.com:ebangin127/naraeon-ssd.git
cd ./naraeon-ssd/ezerase/liveboot/
git clone https://github.com/linux-nvme/nvme-cli.git
wget https://github.com/Drive-Trust-Alliance/exec/blob/master/sedutil_LINUX.tgz?raw=true
mv sedutil_LINUX.tgz?raw=true sedutil.tgz
tar zxvf sedutil.tgz
./naraeon-ssd/ezerase/liveboot/scripts_host/bootstrap.sh