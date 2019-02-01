 [![Travis build status](https://travis-ci.org/ahmedmohamedali/eikonapir.svg?branch=master)](https://travis-ci.org/ahmedmohamedali/eikonapir)
 
# eikonapir
# R wrapper for Refinitiv Eikon API 


### Prerequisites
To use this R api you will need to have  <br />
- **A Refinitiv Eikon License** (please visit https://customers.thomsonreuters.com/eikon/) <br />
- **Refinitiv Eikon4** installed 
or
- The **Refinitiv Eikon API Proxy** installed and running. For more information please visit https://developers.thomsonreuters.com/eikon-apis/eikon-web-and-scripting-apis-limited-access 

## Installation instruction
In the R interpreter type the following commands
```bash
> install.packages("devtools")
```
```bash
> library(devtools)
```
```bash
> install_github("ahmedmohamedali/eikonapir")
```

*Output*:
```bash
* installing *source* package 'eikonapir' ...
** R
** preparing package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded
*** arch - i386
*** arch - x64
* DONE (eikonapir)
```
### Run
- Start **Refinitiv Eikon** or **Refinitiv Eikon API Proxy**
- Run the various test functions in the file R/tests.R 

You can find the API documentation in the **man** directory 

