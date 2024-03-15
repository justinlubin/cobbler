ds_color = xr.open_dataset('https://rsg.pml.ac.uk/thredds/dodsC/CCI_ALL-v4.2-DAILY')
for var in ds_color:
    if not var == 'chlor_a':
        ds_color = ds_color.drop(var)
ds_color
