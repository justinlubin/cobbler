dict_tmp = sa.model_attributes.get_multivariables_with_bounded_sum_by_category(df_cs_integrated, model_energy.modvar_inen_list_fuel_fractions, 1, force_sum_equality=True, msg_append='see XX')
nap = 0
for k in dict_tmp.keys():
    nap += dict_tmp[k]
nap

# *** Dictionary indexing (semi-automatic)

dict_tmp = sa.model_attributes.get_multivariables_with_bounded_sum_by_category(df_cs_integrated, model_energy.modvar_inen_list_fuel_fractions, 1, force_sum_equality=True, msg_append='see XX')
nap = np.sum(dict_tmp.values())
nap

### Modified input:
# dict_tmp = sa.model_attributes.get_multivariables_with_bounded_sum_by_category(df_cs_integrated, model_energy.modvar_inen_list_fuel_fractions, 1, force_sum_equality=True, msg_append='see XX')
# nap = 0
# for v in dict_tmp.values():
#     nap += v
# nap