# Financial Calculator

The purpose of this project is to practise using R by providing a custom implementation of certain basic financial math operations 
both in order to better understand and verify their results and to increase their efficiency. The program works as a 
calculator run from the command line with .csv files as its input. The results are written to another .csv file. 
Each operation is implemented in an .R file of its own. So far, only the operation calculating the IRR has been implemented. 
This was the initial goal of the project, but it can be expanded if the future if needed to encompass more operations like NPV and so on. 

## Download latest version

To get started, download the program [here](https://github.com/caeh17/Financial-Calculator/releases/download/1.0-beta/IRR_calculator.R).

## Manual

### Calculating the IRR for a series of cash flows

To calculate the IRR for a file with cash flow data and another with the current market value for each investment, 
run the following in the command line:

```
Rscript IRR_calculator.R Example_cashflows.csv Example_market_values.csv
```

Where _Example_cashflows.csv_ is the cash flow data and _Example_market_values.csv_ the market value data. To calculate the IRR for data 
in only one .csv file, just ignore the second argument. Example files with fictional financial data can be found in the folder
[Test data](https://github.com/caeh17/Financial-Calculator/tree/master/Test%20data) and can be used to test the program.

**N.B. if the .csv files or the calculator file are not all in the same directory, their appropriate paths should be used instead.
The results file will be created in the same directory as the cash flow file and will bear the same name, except with the text
"-analysis-[date and time]" appended to it. The output file lists all the investments on the same row with their corresponding
IRR calculated on a yearly, monthly and continuous basis.**

There are some some options for the IRR operation that can be passed as an argument in the command line: 

```
Rscript IRR_calculator.R Example_cashflows.csv Example_market_values.csv -R
```

Will uniformly distribute (in the statistical sense) the current market value of each investment monthly over 1 to 5 years, 
the number of years being chosen at random.

```
Rscript IRR_calculator.R Example_cashflows.csv Example_market_values.csv -r 10
```

Similar as above, except the time span for the distribution of the market value is exactly what the user states in the last numeric argument.

```
Rscript IRR_calculator.R Example_cashflows.csv Example_market_values.csv 3
```

Will distribute that market value evenly over each day for as many years as the last argument states.

```
Rscript IRR_calculator.R Example_cashflows.csv Example_market_values.csv -print
```

The -print argument can be added after any of the above options; it will print a log of all the solutions the calculator has
found, including those discarded.

## Testing

The calculator has been mainly tested on the system level and manually. Environments MacOS X and Windows 10 have been used. 
That the IRR is calculated correctly has been verified by calculating it by other means as well as by noting that the continuous
and monthly/yearly IRR values invariably correspond to each other in spite of the different way of arriving at these results. 
