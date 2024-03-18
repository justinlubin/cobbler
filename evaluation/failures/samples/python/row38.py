ICS_DATA_SOURCES = []
for TECHNIQUE in ICS_TECHNIQUES:
    if 'x_mitre_data_sources' in TECHNIQUE.keys():
        for DS in TECHNIQUE['x_mitre_data_sources']:
            if DS not in ICS_DATA_SOURCES:
                ICS_DATA_SOURCES.append(DS)
ICS_DATA_SOURCES
