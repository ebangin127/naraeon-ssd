class InfoGetter:
    def get(self, selected, fixedStorages):
        for device in fixedStorages:
            if selected == device['kname']:
                return device['model'], device['serial']
        return '', ''