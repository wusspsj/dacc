#include "parse.h"
#include "func.h"

extern char dacitem[5][6];
extern struct strlist *strL,*strLT;
extern struct datalist *dataL;
extern struct calclist *calcL;
extern struct shelllist *shellL;
extern struct shapelist *shapeL;
extern FILE *fphost,*fpdev;
extern int icount;

char *defdata = "\tchar *Prog_str;\n\
\tsize_t Prog_len;\n\
\tstruct platformrt *DAC_platforms;\n\
\tstruct kr *DAC_krt;\n\
\tstruct kroa *DAC_koa0,*DAC_koa1;\n\
\tstruct krda *DAC_kda0,*DAC_kda1;\n";

char *defenv = "\n\tProg_str = prog_Src(\"src/dac/kernel.cl\",&Prog_len);\n\
\tDAC_platforms = chk_Env(Prog_str,Prog_len,NULL);\n";

int dacocl(char *newstr,char *end,char *fname)
{
	struct strlist *p;
	char *cur,*hname,*kname;
	size_t size;
	int flag1,flag2;
	struct shapelist *shp;
	struct datalist *dp;

	size = strlen(fname);
	hname = (char *)malloc(sizeof(char)*size + 7);
	kname = (char *)malloc(sizeof(char)*size + 10);
	strcpy(hname,fname);
	strcat(hname,"host.c");
	strcpy(kname,fname);
	strcat(kname,"kernel.cl");

    if((fphost = fopen(hname,"wb+")) == NULL)
    {
        printf("Failed to create file :%s\n",hname);
        exit(0);
    }
    if((fpdev = fopen(kname,"wb+")) == NULL)
    {
        printf("Failed to create file :%s\n",kname);
        exit(0);
    }
	//start translating to ocl
	fprintf(fphost,"#include \"dacrt.h\"\n");
    cur = newstr + 1;
	p = strL;
	shp = shapeL;
	dp = dataL;
	flag1 = 0;
	flag2 = 0;
	while(cur < end)
	{
		if(p != NULL)
		{
			if(cur < p->str)   //copy normal string
			{
				size = p->str - cur;
				//printf("write %d\n",size);
				if(fwrite(cur,size,1,fphost) != 1)
				{
					printf("failed to write host file\n");
					exit(0);
				}
//				printf("write host\n");
				//exit(0);
			}
			//deal with special str
			switch(p->type)
			{
			case 0:
				//deal computation
				//here to build opencl run env;select device,progsrc
				dealcomptOCL(p);
				printf("dealcompt end\n");
				break;
			case 1:
				if(flag2 == 0)
				{
					fprintf(fphost,"%s",defdata);
					flag2 = 1;
				}
				if(dp != NULL)  //here to init struct krt and platform
					dp = dealdataOCL(dp);
				if(flag1 == 0)
				{
					fprintf(fphost,"%s",defenv);
					flag1 = 1;
				}
				break;
			case 2:
				//deal shape
//				printf("dealshape\n");
				dealshapeOCL(shp);
				shp = shp->next;
				printf("dealshape end\n");
				break;
            case 3:
				dealrwOCL(p);
				printf("dealrw end\n");
				break;
			case 6:
				//deal free
				printf("dealfree\n");
				dealfreeOCL();
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
    fclose(fphost);
	fclose(fpdev);
	printf("OCL generation done!\n");
	return 0;
}
