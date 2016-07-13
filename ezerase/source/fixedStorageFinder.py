class FixedStorageFinder:
    def __isFixed(self, device):
        return (device['model'] is not None) & (device['vendor'] is not None) & (device['type'] == 'disk')
    def __mergeVendorModel(self, device):
        device['model'] = (device['vendor'] + device['model']).strip()
        del device['vendor']
        return device
    def find(self, objectoutput):
        blockdevices = objectoutput['blockdevices']
        fixedstorages = list()
        for device in blockdevices:
            if(self.__isFixed(device)):
                mergedDevice = self.__mergeVendorModel(device)
                fixedstorages.append(mergedDevice)
        return fixedstorages
