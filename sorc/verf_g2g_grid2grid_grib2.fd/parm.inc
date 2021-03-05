	PARAMETER (maxmod = 25,mxfcst = 32,mxdate = 32,maxobs = 32)
	PARAMETER (mxarea = 35,mxstat = 10,mxvrbl = 20,maxlvl = 20)  !mxvrbl can not be too big!
	PARAMETER (mxb = 20)
	PARAMETER (maxj = 110)
	PARAMETER (factor = 3.1415926 / 180.0)
        PARAMETER (maxpts = 3000000)      !for g2g grid points
        PARAMETER (maxfho = 20 )          !max # of FHO thresholds

C Note:
c   if mxvrbl is set too large (e.g. mxvrbl=25), then compilation will
c  get  warning: "remark #6375: Because of COMMON, the alignment of object is inconsistent with its type - potential performance impact"
c
c  mxfcst and maxobs must be same value 
c
c

