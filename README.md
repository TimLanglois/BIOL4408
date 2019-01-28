# BIOL4408
Marine Ecology Rottnest field trip


#Data analysis
We are going to use R for checking and plotting data and PRIMER/PERMANOVA for statistical tests.


To run R we are going to use an RStudio server through a new service called ecocloud.
Each of you will have your own server and your own permanent workspace on ecocloud (10gb).



#Logging into ecocloud
Go to app.ecocloud.org.au
Log in using AAF (which will use your University credentials)

#Starting a Server
Once logged in you will be on the Dashboard
Go to Tools page
Click 'R (RStudio and Jupyter)' and follow the prompts to start an R Server. This will set you up on a Virtual Machine in the cloud


#Start RStudio and set working directory
Once your Server has started you will be on the JupyterLab dashboard
Click on the RStudio box on the bottom right. This will open an RStudio session in a new tab.

Set your working directory to your 'workspace' folder by one of the below options:
Type setwd("~/workspace") into your console and hit enter, 
OR
In the files panel (bottom right hand side of screen) click on the workspace folder, and then click on the More button and select Set As Working Directory

This workspace folder is permanent, and will not be deleted even when you shutdown your server. You can also access this folder and it’s contents from the ecocloud website.


#Access data from GitHub from within RStudio:
Access a GitHub repository and download your code/data
This step will download and save your data and files to your workspace so that they are available across the server

Open the Terminal window within RStudio
Type the following command:
cd workspace  "hit RETURN""
git clone https://github.com/TimLanglois/BIOL4408.git <RETURN>

If you need to update the code/data from GitHub
Type the following command:
cd BIOL4408 <RETURN>
git pull <RETURN>

BUT BE CAREFUL - git pull will overwrite any changes you have made to the scripts.

I suggest you make you own version of the code in the Analysis folders that you can edit and annotate

