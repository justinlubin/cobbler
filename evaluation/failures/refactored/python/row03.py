PHS_turnaround_efficiency_average = 0
for ds in PHS_turnaround_efficiency:
    PHS_turnaround_efficiency_average += ds['turnaround efficiency'] / len(PHS_turnaround_efficiency)
PHS_turnaround_efficiency_average

# *** Dictionary indexing (semi-automatic)

intermediate = [ds['turnaround efficiency'] for ds in PHS_turnaround_efficiency]
PHS_turnaround_efficiency_average = np.sum(np.divide(intermediate, np.full(len(intermediate), len(PHS_turnaround_efficiency))))
PHS_turnaround_efficiency_average

### Modified input:
# intermediate = [ds['turnaround efficiency'] for ds in PHS_turnaround_efficiency]
# PHS_turnaround_efficiency_average = 0
# for i in intermediate:
#     PHS_turnaround_efficiency_average += i / len(PHS_turnaround_efficiency)
# PHS_turnaround_efficiency_average