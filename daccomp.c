#include "parse.h"
#include "func.h"

extern char dacitem[5][6];
extern struct strlist *strL,*strLT;
extern struct datalist *dataL;
extern struct calclist *calcL;
extern struct shelllist *shellL;
extern struct shapelist *shapeL;
extern FILE *fphost,*fpkernel,*fpdacrt;
extern int icount,spEn;

char *defspvar = "int dac_tn,dac_nts;\n";

char *dacrt = "\nstruct dss{\n\
\tsize_t type;\n\
\tsize_t size;\n\
\tint dim;\n\
\tsize_t *range;\n\
\tsize_t *range_u;\n};\n";

int dacomp(char *newstr,char *end,char *fname)
{
	struct strlist *p;
	char *nstr,*cur,*cmp,*hname,*kname;
	size_t slen,size;
	int count;
	struct shapelist *shp;
	struct datalist *dp;

    size = strlen(fname);
	hname = (char *)malloc(sizeof(char)*size + 6);
	kname = (char *)malloc(sizeof(char)*size + 6);
	strcpy(hname,fname);
	strcat(hname,"omp.c");
	strcpy(kname,fname);
	strcat(kname,"omp.h");

	if((fphost = fopen(hname,"wb+")) == NULL)
    {
        printf("Failed to create file :%s\n",hname);
        exit(0);
    }
    if((fpkernel = fopen("kernels.c","wb+")) == NULL)//kernel.c as temp,removed after copy
    {
        printf("Failed to create file :kernels.c\n");
        exit(0);
    }
    if((fpdacrt = fopen(kname,"wb+")) == NULL)
    {
        printf("Failed to create file :%s\n",kname);
        exit(0);
    }
	//start translating to omp
    //head file and extra data surverillance structure injection
	fprintf(fphost,"#include <omp.h>\n");
	fprintf(fphost,"#include \"dacomp.h\"\n");
    cur = newstr + 1;
	p = strL;
	shp = shapeL;
	//ref = refL;
	dp = dataL;
 //   printf("generate output files\n");
	while(cur < end)
	{
		if(p != NULL)
		{
			if(cur < p->str)   //copy normal string
			{
				size = p->str - cur;
				if(p->type == 7)
                {
                    cmp = strstr(cur,"stdio.h");
                    if((cmp == NULL)||(cmp > p->str))
                    {
                        fprintf(fpdacrt,"#include <stdio.h>\n");
                    }
                    if(fwrite(cur,size,1,fpdacrt) != 1)
                    {
                        printf("failed to write host file\n");
                        exit(0);
                    }
                    fprintf(fpdacrt,"%s\n",dacrt);
                }
                else{
                    if(fwrite(cur,size,1,fphost) != 1)
                    {
                        printf("failed to write host file\n");
                        exit(0);
                    }
                }
				//printf("write %d\n",size);
			}
			//deal with special str
			switch(p->type)
			{
			case 0:
				//deal computation
				dealcomptOMP(p);
				printf("dealcompt end\n");
				break;
			case 1://data
				if(dp != NULL)  //here to init struct krt and platform
					dp = dealdataOMP(dp);
                if(icount > 0)
                {
                    fprintf(fphost,"int dac_i0");
                    for(count=1;count<icount;count++)
                    {
                        fprintf(fphost,",dac_i%d",count);
                    }
                    fputc(';',fphost);
                    fputc('\n',fphost);
                    icount = -1;
                }
                if(spEn > 0)
                {
                    fprintf(fphost,"%s",defspvar);
                    spEn = -1;
                }
				break;
			case 2:
				//deal shape
//				printf("dealshape\n");
				dealshapeOMP(shp);
				shp = shp->next;
				printf("dealshape end\n");
				break;
            case 3:
				dealrwOMP(p);
				printf("dealrw end\n");
				break;
			case 6://end of main
				//deal free
				printf("dealfree\n");
				dealfreeOMP();
				break;
			default:              //skip data,shell,calc,dedup string
//				printf("default %d\n",p->type);
				break;
			}
			//resize cur
			cur = p->str + p->len;
			p = p->next;
		}
		else{              //special string done,copy the rest
			size = end - cur;
//			printf("last %Iu\n",size);
			if(fwrite(cur,size,1,fphost) != 1)
			{
				printf("failed to write host file\n");
				exit(0);
			}
			break;
		}
	}
	//merge fphost and fpkernel
	fseek(fpkernel, 0, SEEK_END);
    slen = ftell(fpkernel);
 //   printf("%d\n",slen);
    fseek(fpkernel, 0, SEEK_SET);
    nstr = (char *)malloc(slen + 1);
    if(fread(nstr,slen,1,fpkernel) != 1)
    {
        fclose(fpkernel);
        free(nstr);
        printf("Read kernel file failed!\n");
        exit(0);
    }
    // close the file and return the total length of the combined string
    fclose(fpkernel);
    nstr[slen] = '\0';
    if (fwrite(nstr,slen,1,fphost) != 1)
    {
        fclose(fphost);
        free(nstr);
        printf("copy kernel to host file failed!\n");
        exit(0);
    }

	count = remove("kernels.c");
	if(count != 0)
        printf("Error in removing kernel file, errNum:%d\n",count);
    //rm kernel file
    free(nstr);
    fclose(fphost);
	fclose(fpdacrt);
    printf("OMP generation done!\n");
	return 0;
}
