


# this script smooths population estimates reported for 5-year age groups into population estimates for 1-year age groups
# together, the functions below calculate three different interpolation techniques: k-order polynomial, linear, and spline interpolation

# pip install pandas numpy scipy statsmodels matplotlib
import numpy as np
import pandas as pd
from scipy.interpolate import CubicSpline, interp1d
import statsmodels.api as sm
import matplotlib.pyplot as plt

# load 5-year age group counts for the population
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000501
census = pd.DataFrame({  # population totals, 2025
    "age": [
        "0-4", 
        "5-9", 
        "10-14", 
        "15-19", 
        "20-24", 
        "25-29", 
        "30-34", 
        "35-39",
        "40-44", 
        "45-49", 
        "50-54", 
        "55-59", 
        "60-64", 
        "65-69", 
        "70-74",
        "75-79", 
        "80-84", 
        "85-89", 
        "90-94", 
        "95-99",
    ],
    "pop": [
        1871184, 
        2138350, 
        2251628, 
        2319393, 
        2703780, 
        2995647,
        3175724,
        3032523, 
        2859631, 
        2593361,
        2447311, 
        2446446,
        2708208,
        2494286,
        2044533, 
        1609542,
        1008273,
        579797, 
        275677, 
        84078,
    ],
})

# parese and split the strings for the age brackets to identify midpoints in age ranges
def _split_age_brackets(data):s
    """parse age strings and derive age bracket midpoint (center) and width to impute populations for 5-year age brackets."""

    # stop if the age column is not string/object
    if data["age"].dtype != object:
        raise TypeError("The age column must be of dtype object/str, e.g. '0-4', '5-9', etc.")
    # stop if the column that lists the population is not numeric
    if not pd.api.types.is_numeric_dtype(data["pop"]):
        raise TypeError("The pop column must be numeric.")

    # split the string on common delimiters
    split = data["age"].str.split(r"[-/_;, ]", regex=True, expand=True).astype(float)
    split.columns = ["lo", "hi"]

    # join split numeric columns to original data
    out = pd.concat([split, data.reset_index(drop=True)], axis=1)

    # find the center points for the 5-year age brackets i.e., the middle age groups in the numeric coding
    out["center"] = (out["hi"] + out["lo"]) / 2
    out["width"] = (out["hi"] - out["lo"]).abs() + 1
    return out


# 1) function to compute polynomial interpolation ---------------------------------
def polynomial(data, k=1):
    """note that a 1-order polynomial is equivalent to linear interpolation."""

    data = _split_age_brackets(data)

    x = data["center"].values
    y = (data["pop"] / data["width"]).values  # smooth

    # k-degree polynomial term
    X = np.column_stack([x**d for d in range(1, k + 1)])
    X = sm.add_constant(X)
    model = sm.OLS(y, X).fit()
    print(model.summary()) # display equation

    # range
    lo, hi = int(data["lo"].min()), int(data["hi"].max())
    ages = np.arange(lo, hi + 1)

    # estimate census counts by age
    X_new = np.column_stack([ages**d for d in range(1, k + 1)])
    X_new = sm.add_constant(X_new, has_constant="add")
    pop_pred = model.predict(X_new)

    newdata = pd.DataFrame({"age": ages, "pop": pop_pred})
    # don't run
    # newdata["pop"] = newdata["pop"].round(0)  # round to whole number

    print("age distribution:")
    print(newdata.to_string(index=False))
    return newdata


census_poly = polynomial(data=census, k=5)


# 2) function for linear/spline interpolation ----------------------------------------------------------
def interpolation_fun(data, method="spline"):
    data = _split_age_brackets(data)

    # range
    lo, hi = int(data["lo"].min()), int(data["hi"].max())
    ages = np.arange(lo, hi + 1)

    x = data["center"].values # the center points
    y = (data["pop"] / data["bracket_width"]).values # smooth

    if method == "spline":
        model = CubicSpline(x, y, bc_type="not-a-knot")
        pop_pred = model(ages)
    elif method == "linear":
        model = interp1d(x, y, kind="linear", fill_value="extrapolate")
        pop_pred = model(ages)
    else:
        raise ValueError("method must be 'spline' or 'linear'")

    newdata = pd.DataFrame({"age": ages, "pop": pop_pred})
    # don't run
    # newdata["pop"] = newdata["pop"].round(0)  # round to whole number

    print("age distribution:")
    print(newdata.to_string(index=False))
    return newdata


census_spline = interpolation_fun(data=census, method="spline")
census_linear = interpolation_fun(data=census, method="linear")


# compare census estimates by interpolation strategy ---------------------------
def test(data, test_):
    x = data.iloc[:, 1]
    y = test_.iloc[:, 1]
    r = np.corrcoef(x, y)[0, 1] # correlation coefficient
    print(f"correlation coefficient = {round(r, 2)}")


# run each line separately
test(data=census_poly, test_=census_spline)
test(data=census_poly, test_=census_linear)
test(data=census_spline, test_=census_linear)


# plot the age-graded population estimates
fig, ax = plt.subplots(figsize=(10, 5))
ax.plot(census_spline["age"], census_spline["pop"], color="firebrick", linewidth=1.5, label="Spline")
ax.plot(census_linear["age"], census_linear["pop"], color="blue", linewidth=1.5, label="Linear")
ax.plot(census_poly["age"], census_poly["pop"], color="forestgreen", linewidth=1.5, label="Polynomial")

ax.set_xlim(0, 100)
ax.set_ylim(0, 1_000_000)
ax.set_xticks(range(0, 101, 10))
ax.set_yticks(range(0, 1_000_001, 100_000))
ax.yaxis.set_major_formatter(lambda val, pos: f"{int(val):,}")

# title and subtitle, both placed in the same (axes) coordinate system so
# spacing between them stays tight and consistent
ax.text(0, 1.11, "Population estimates on July 1, 2025, Canada",
        transform=ax.transAxes, ha="left", fontsize=16, color="black")
ax.text(0, 1.045, "Triangulation of interpolation methods",
        transform=ax.transAxes, ha="left", fontsize=12, color="black")

ax.set_xlabel("Age", fontsize=12, color="black")
ax.set_ylabel("Population", fontsize=12, color="black")
ax.tick_params(labelsize=10, colors="black")

ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.spines["left"].set_color("black")
ax.spines["bottom"].set_color("black")

legend = ax.legend(title="Method", loc="upper right", frameon=True)
legend.get_frame().set_edgecolor("black")
legend.get_frame().set_facecolor("white")
plt.setp(legend.get_texts(), color="black")
plt.setp(legend.get_title(), color="black")

# extra top margin reserved via rect so the title/subtitle aren't clipped
plt.tight_layout(rect=[0, 0, 1, 0.90])

# output high resolution figure
plt.savefig("~/Desktop/fig_01.png", dpi=250)
plt.close()



# close script


