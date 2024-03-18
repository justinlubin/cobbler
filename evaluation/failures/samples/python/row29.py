from sklearn.feature_selection import SelectFromModel
from sklearn.linear_model import LassoCV
selector = SelectFromModel(LassoCV())
selector.fit(x, y)
support = selector.get_support()
best_features = x.loc[:, support].columns.tolist()
print(str(len(best_features)), 'selected features')
print(best_features)
df_preprocessed_lasso = x
for i in x.columns:
    if i not in best_features:
        df_preprocessed_lasso = df_preprocessed_lasso.drop(columns=[i], axis=1)
df_preprocessed_lasso
