#include "parse.h"
#include "func.h"

char dacitem[5][6] = {
	"data",
	"shape",
	"rw",//read write
	"shell",
	"calc"
};

struct strlist *strL,*strLT;
struct datalist *dataL;
struct calclist *calcL;
struct shelllist *shellL;
struct shapelist *shapeL;
FILE *fphost,*fpkernel,*fpdacrt,*fpdev;
int icount,spEn;

int main(int argc,char *argv[])
{
    FILE *pFile;//,*fpdac;
	char *newstr,*cur,*end;
	char fname[10];
	size_t offset,slen;
	int strtype,count,i;
	int oclEn,ompEn;

	oclEn = 0;
	ompEn = 0;
	if(argc < 2)
	{
		printf("Please input one and only one filename\n");
		exit(0);
	}
	else if(argc > 3)
    {
        printf("Too many command line parameters!\n");
		exit(0);
    }
    if(argc == 2)
    {
        i = 1;
        oclEn = 1;
        ompEn = 1;
    }
    else{
        i = 2;
        if(strcmp(argv[1],"-ocl") == 0)
        {
            oclEn = 1;
        }
        else if(strcmp(argv[1],"-omp") == 0)
        {
            ompEn = 1;
        }
        else{
            printf("Illegal command line parameters, wrong code version selection!\n");
            exit(0);
        }
    }
    //record filename
    for(count=0;count<9;count++)
    {
        if((argv[i][count] != '.')&&(argv[i][count] != '\0'))
        {
            fname[count] = argv[i][count];
        }
        else{
            break;
        }
    }
    fname[count] = '\0';
    if((pFile = fopen(argv[i],"rb")) == NULL)
    {
        printf("Failed to open file :%s\n",argv[i]);
        exit(0);
    }

    // get the length of the source code
    fseek(pFile, 0, SEEK_END);
    slen = ftell(pFile);
    fseek(pFile, 0, SEEK_SET);
    // allocate a buffer for the source code string and read it in
    newstr = (char *)malloc(slen + 2);
    newstr[0] = '\n';
    //memcpy(clnewstr, clProgramStr, srcLength);
    if (fread(newstr+1,slen,1,pFile) != 1)
    {
        fclose(pFile);
        free(newstr);
        printf("Read program source file failed!\n");
        exit(0);
    }
    // close the file and return the total length of the combined string
    fclose(pFile);
    newstr[slen+1] = '\0';
    printf("Src end\n");
    cur = newstr+1;

	//remove all the comments
	rmcomment(cur,&slen);// does it necessary?
	printf("remove comments ends\n");

	icount = 0;
	spEn = 0;
	end = cur + slen;
    while(cur < end)
    {
    	if(*cur == 'm')
		{
			if((*(cur+1) == 'a')&&(*(cur+2) == 'i')&&(*(cur+3) == 'n'))
			{
				if((chkchar(*(cur-1)) == 1)&&(chkchar(*(cur+4)) == 1))
				{
//				    printf("main\n");
					offset = checkmain(cur);
					printf("checkmain end\n");
					cur += offset+1;
				}
			}
		}
		else if(*cur == 'D')
    	{
			strtype = getstrtype(cur,1);
//			printf("gettype %d\n",strtype);
			//treat shell,calc
			if(strtype > 2)
			{
				offset = 0;
				count = 0;
				while(cur[offset] != '\0')
				{
					if(cur[offset] == '{')
						count++;
					if(cur[offset] == '}')
					{
						count--;
						if(count == 0)
						{
							break;
						}
					}
					offset++;
				}
				switch(strtype)
				{
					case 4:
						printf("signshell start...");
						signshell(cur,offset);
						printf("signshell end\n");
						break;
					case 5:
						printf("signcalc start...");
						signcalc(cur,offset);
						printf("signcalc end\n");
						break;
					default:
						break;
				}
				offset++;
//				printf("ss s\n");
				signstr(cur,offset,strtype);
				//printf("ss e\n");
				cur += offset;
			}
		}
		cur++;
	}
	printf("pass1 end\n");

    if(oclEn == 1)
    {
        printf("Start generating OpenCL code!!\n");
        dacocl(newstr,end,fname);
    }
    if(ompEn == 1)
    {
        printf("Start generating OpenMP code!!\n");
        dacomp(newstr,end,fname);
    }

//	printf("start free\n");
    free(newstr);
    free(strL);
    free(dataL);
    free(calcL);
    free(shellL);
    free(shapeL);
	printf("All done!\n");
    return 0;
}
