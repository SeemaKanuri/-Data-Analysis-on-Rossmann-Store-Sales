import subprocess
a=input("Enter your name to proceed with analysis: ")
if (a=="gaurav"):
    #subprocess.call("D:\\scripting project\\library.R", shell=True)
    #subprocess.call("D:\\scripting project\\Net.R", shell=True)
    subprocess.call("D:\\scripting project\\Analysis.R", shell=True)
elif(a=="naveen"):
    subprocess.call("D:\scripting project\\StoreWise.R", shell=True)
elif (a=="seema"):
    subprocess.call("D:\scripting project\\store_Type_analysis.R", shell=True)
elif(a=="goutham"):
    subprocess.call("D:\scripting project\\Store_Performance.R", shell=True)
else:
    print("Invalid User")
    
    
    
