#include "parse.h"
#include "func.h"

#define Fwd 1
#define Bwd -1

extern struct strlist *strL,*strLT;
extern struct datalist *dataL;
extern struct calclist *calcL;
extern struct shelllist *shellL;
extern struct shapelist *shapeL;
extern FILE *fphost,*fpkernel,*fpdacrt;

char *dealsp1 = "\tdac_tn = omp_get_thread_num();\n\
\tdac_nts = omp_get_num_threads();\n";

int chkop(char *c,int f)
{
    if((*c == '+')||(*c == '-')||(*c == '*')||(*c == '/')||(*c == '%'))
    {
        return 1;
    }
    else if((*c == '&')||(*c == '|')||(*c == '^'))
    {
        return 1;
    }
    else if(*c == '<')
    {
        if(*(c+f) == '<')
            return 1;
        else
            return 0;
    }
    else if(*c == '>')
    {
        if(*(c+f) == '>')
            return 1;
        else
            return 0;
    }
    else
        return 0;
}

//return the serial num of a kernel ??how to tell it's a old one or a new one ? unless we create a new one before we return the num
int chkkernel(struct reflist *re0, struct calclist *cal, struct shelllist *sh)  // dd: return a kernel name,check kernel should be here,because kernel structure is built in compilation time.
{
	struct reflist *re;
	struct kernellist *ker,*kp;
	struct typelist *tp,*tq;
	struct strlist *sp,*sq,*stmp,*scluster,*scp,*scq;
	struct varlist *vp,*vq;
	struct datalist *vars,*varsq;//vars: calc variables
	struct rangei *rip;
	struct indexlist *ip;
	struct splist *spl;
//	struct atomlist *ap0,*ap,*aq;
//	struct strcluster *sc0,*scp,*scq;
	/*struct posprop *pp;*/
	int i,j,k,count,idxn,spn,*ids;//flag not needed, idxn: the number of chosen index, spn: chosen sp
	char *cur,*end,*s,*stsc,*edsc,*sc;
	size_t size;
	count = 1;  //kernel count start from 1
	ker = cal->ker;
	kp = cal->ker;
//	printf("chkk\n");
	while(ker != NULL)
	{
		if(strcmp(ker->sname,sh->shname) == 0)
		{
			i = 0;
			tp = ker->t;
			re = re0;
			while((re != NULL)&&(tp != NULL))
			{
				if(strcmp(re->ref->type,tp->type) != 0)
				{
					i = 1;
					break;
				}
				re = re->next;
				tp = tp->next;
			}
			if(i == 0)
				return count;//kernel exists,return kernel id
		}
		kp = ker;//kp point to the tail of kernel list
		ker = ker->next;
		count++;
	}
//	printf("chkk1\n");
	//new kernel
	ker = (struct kernellist *)malloc(sizeof(struct kernellist));
	ker->next = NULL;
	ker->t = NULL;
	ker->count = count;
	ker->sname = sh->shname;
	re = re0;
	while(re != NULL)
	{
		tp = (struct typelist *)malloc(sizeof(struct typelist));
		tp->next = NULL;
		tp->type = re->ref->type;
		if(ker->t == NULL)
		{
			ker->t = tp;
			tq = tp;
		}
		else{
			tq->next = tp;
			tq = tp;
		}
		re = re->next;
	}
	if(kp == NULL)
		cal->ker = ker;
	else
		kp->next = ker;
//	printf("chkk2\n");
	//new kernel here
	fprintf(fpkernel,"\nvoid %s%d(",cal->cname,count);//kernel name
	fprintf(fpdacrt,"\nvoid %s%d(",cal->cname,count);
	//kernel args,data objects first,then deal calc strings one by one

	//normal args first
	sp = cal->strl;
	if(sp->type == 10)
	{
		if(fwrite(sp->str,sp->len,1,fpkernel) != 1)
		{
			printf("failed to write dev file\n");
			exit(0);
		}
		fputc(',',fpkernel);
		if(fwrite(sp->str,sp->len,1,fpdacrt) != 1)
		{
			printf("failed to write kernel file\n");
			exit(0);
		}
		fputc(',',fpdacrt);
		sp = sp->next;
	}
	//data objects
	re = re0;
	vars = cal->vars;
	while(re != NULL)  //pass all the dims as args of the kernel
	{
		fprintf(fpkernel,"%s *%s_h, struct dss %s, ",re->ref->type,vars->dname,vars->dname);
		fprintf(fpdacrt,"%s *, struct dss, ",re->ref->type);
		re = re->next;
		vars = vars->next;
	}

	if(sh->inum != 0)
	{
	    fprintf(fpkernel,"int dac_i0");
	    fprintf(fpdacrt,"int");
		for(i=1;i<sh->inum;i++)
        {
            fprintf(fpkernel,", int dac_i%d",i); //exceeded index variables, treat as outer control
            fprintf(fpdacrt,", int");
        }
	}
	else{
        fprintf(fpkernel,"int dac_tn,int dac_nts");
        fprintf(fpdacrt,"int ,int ");
	}

    fprintf(fpkernel,")\n{");
	fprintf(fpdacrt,");\n");

	cur = cal->start+1;
	end = cur + cal->clen;
    //data reference aggregation, according to the vp->ioid in shell
    scp = NULL;
    if(sh->ionum > 1)
    {
        stmp = sp;//store sp in tmp
        while((sp != NULL)&&(sp->type == 12))//.type
        {
            sq = sp;
            sp = sp->next;
        }
        while(sp != NULL)
        {
            j = -1;
            scluster = (struct strlist *)malloc(sizeof(struct strlist));
            scluster->type = 30;
            scluster->len = 0;
            scluster->next = NULL;
            while(sp != NULL)
            {
                vp = sh->vars;
			//re = re0;
                for(i=1;i<sp->len;i++)      //the serial num start from 0 or 1?CHECK IT! => start from 1. vp initialization?
                {
                    vp = vp->next;
                    //re = re->next;
                }
                if(j == -1)
                    j = vp->ioid;
                if(vp->ioid == j)
                    scluster->len++;
                if((vp->ioid != j)||(sp->next == NULL))//new cluster or end of list
                {
                    if((scluster->len == 1)&&(sq->type == 13))//only a .range expression
                    {
                        free(scluster);
                        break;
                    }
                    //search up bound,set scluster->str
                    count = 0;
                    if(scluster->len > 1)
                    {
                        stsc = scluster->next->str;
                        if(vp->ioid != j)
                            edsc = sq->str;
                        else
                            edsc = sp->str;
                        while(stsc < edsc)
                        {
                            if(*stsc == '}')
                                count++;
                            else if(*stsc == '{')
                                count--;
                            stsc++;
                        }
                    }

                    stsc = scluster->next->str;
                    while(stsc > scq->str)
                    {
                        if(*stsc == ';')
                        {
                            if(count <= 0)
                                break;
                        }
                        else if(*stsc == '}')
                        {
                            if(count <= 0)
                                break;
                            else
                                count++;
                        }
                        else if(*stsc == '{')
                            count--;
                        stsc--;
                    }

                    if(stsc == scq->str)
                        printf("Alert!!! Error in finding the upper bound in data aggregation operation!!!\n");
                    scluster->str = stsc + 1;
                    //set mask
                    re = re0;
                    for(i=1;i<sp->len;i++)
                        re = re->next;
                    scluster->len = re->mask; //check this!!!!!!
                    //insert scluster
                    if(scp == NULL)
                        scp = scluster;
                    else
                        scq->next = scluster;
                    scq = scluster;

                    //search lower bound
                    if(vp->ioid != j)
                    {
                        stsc = sq->str;
                        edsc = sp->str;
                    }
                    else{
                        stsc = sp->str;
                        edsc = cal->start + cal->clen;
                    }
                    while(stsc > edsc)
                    {
                        if((*stsc == ';')&&(count == 0))
                            break;
                        else if(*stsc == '}')
                        {
                            if(count == 0)
                                break;
                            else
                                count++;
                        }
                        else if(*stsc == '{')
                            count--;
                        stsc++;
                    }
                    //new scluster
                    if(stsc == edsc)
                        printf("Alert!!! Error in finding the upper bound in data aggregation operation!!!\n");
                    scluster = (struct strlist *)malloc(sizeof(struct strlist));
                    scluster->str = stsc + 1;// OCL need modify
                    scluster->type = 32;//32--> }
                    scluster->len = 0;//8--> } in OpenCL
                    scluster->next = NULL;
                    scq->next = scluster;
                    scq = scluster;
                    if(vp->ioid != j)
                    {
                        break;
                    }
                }
                sq = sp;
                sp = sp->next;
            }
        }
        sp = stmp;//restore sp
    }
    //merge scp into sp
    if(scp != NULL)
    {
        sp = cal->strl;
        while((sp != NULL)&&(scp != NULL))
        {
            if(sp->str >= scp->str)
            {
                sq->next = scp;
                sq = scp;
                scp = scp->next;
                sq->next = sp;
            }
            else{
                sq = sp;
                sp = sp->next;
            }
        }
        if((sp == NULL)&&(scp != NULL))
        {
            sq->next = scp;
        }
    }

    //check atomic
    sp = cal->strl;
    scp = NULL;
    while(sp != NULL)
    {
        if(sp->type == 11)
        {
            vp = sh->vars;
            vars = cal->vars;
            re = re0;
            for(i=1;i<sp->len;i++)      //the serial num start from 0 or 1?CHECK IT! => start from 1. vp initialization?
            {
                vp = vp->next;
                vars = vars->next;
                re = re->next;
            }
            count = 1;
            if(sh->inum > 0)
            {
                count <<= sh->inum;
                count--;
            }
            if(((vp->sis == 1)&&(vp->ddname == NULL))||((re->mask | re->idx) < count))
            {
                edsc = sp->str;
                while(edsc < end)
                {
                    if(*edsc == '{')
                    {
                        i = 1;
                        while(edsc < end)
                        {
                            if(*edsc == '{')
                                i++;
                            if(*edsc == '}')
                            {
                                i--;
                                if(i == 0)
                                    break;
                            }
                            edsc++;
                        }
                        break;
                    }
                    if(*edsc == ';')
                        break;
                    edsc++;
                }
                //printf("edsc: %c, %d\n",*edsc,edsc);
                stsc = sp->str;
                while(stsc > cal->start)
                {
                    if((*stsc == ';')||(*stsc == '{')||(*stsc == '}')||((*stsc == '\n')&&(*(stsc-1) != '\\')))
                        break;
                    else
                        stsc--;
                }
                //printf("stsc: %c, %d\n",*stsc,stsc);
                stsc++;
                //start check atomic type, using j to store atomic type
                j = 0;//-1:critical,1:read,2:write,3:update,4:capture,5:capture block
                if(*edsc != ';')//not ended with ";"->not a single expression,must be critical
                {
                    j = -1;
                }
                else{//check if atomic
                    s = stsc;
                    i = 0;
                    while(s < edsc)//find the first "="
                    {
                        if((*s == ';')||(*s == '\n')||(*s == '{'))
                            break;
                        if((*s == '=')&&(*(s+1) != '=')&&(*(s-1) != '<')&&(*(s-1) != '>'))
                        {
                            i = 1;
                            break;
                        }
                        s++;
                    }
                    if(i == 0)//no "=", check if ++ -- only
                    {
                        s = sp->str - 1;
                        if(((*s == '+')&&(*(s-1)== '+'))||((*s == '-')&&(*(s-1) == '-')))
                        {
                            i++;
                            s -= 2;
                        }
                        i++;
                        while(s > stsc)
                        {
                            if(chkchar(*s) == 0)
                            {
                                printf("tp-0 : %c\n",*s);
                                i = 0;
                                break;
                            }
                            s--;
                        }
                        if(i > 0)
                        {
                            s = sp->str + strlen(vars->dname);
                            count = vars->dim;
                            while((count > 0)&&(s <= edsc))
                            {
                                if(*s == ']')
                                    count--;
                                s++;
                            }

                            if(s > edsc)
                            {
                                printf("wrong data dim in calc\n");
                                exit(0);
                            }
                            if(((*s == '+')&&(*(s+1)== '+'))||((*s == '-')&&(*(s+1) == '-')))
                            {
                                i++;
                                s += 2;
                            }
                            while(s < edsc)
                            {
                                if(chkchar(*s) == 0)
                                {
                                    printf("tp-1 : %c\n",*s);
                                    i = 0;
                                    break;
                                }
                                s++;
                            }
                        }
                        if(i == 2)
                        {
                            j = 3;//update
                        }
                        else{
                            j = -1;//critical
                        }
                    }
                    else{//"=" exists
                        //s signs the location of "="
                        if(s > sp->str)//data ref on the left
                        {
                            //check other data ref
                            i = 0;
                            sq = sp->next;
                            while((sq != NULL)&&(sq->str < edsc))
                            {
                                if(sq->len == sp->len)
                                {
                                    i++;
                                    if(i == 2)
                                        break;
                                }
                                else{
                                    i = 3;
                                    break;
                                }
                                sq = sq->next;
                            }
                            if(i > 1)
                            {
                                j = -1;//critical
                            }
                            else if(i == 0)//no other data ref in the same line
                            {
                                sc = s-1;
                                if(chkop(sc,Bwd))
                                {
                                    j = 3;
                                }
                                else{
                                    j = 2;
                                }
                            }
                            else{//i == 1;
                                //check
                                sc = s + 1;
                                sq = sp->next;
                                i = 0;
                                while(sc < sq->str)
                                {
                                    if(*sc == '(')
                                    {
                                        i++;
                                    }
                                    else if(*sc == ')')
                                    {
                                        i--;
                                    }
                                    sc++;
                                }
                                if(i > 0)
                                {
                                    j = -1;//critical
                                }
                                else{
                                    sc = s+1;
                                    i = 0;
                                    while(sc < sq->str)
                                    {
                                        if(*sc != ' ')
                                        {
                                            i = 1;
                                            break;
                                        }
                                        else
                                            sc++;
                                    }
                                    if(i == 0)
                                    {
                                        //Fwd
                                        sc = sq->str + strlen(vars->dname);//sq->len == sp->len
                                        count = vars->dim;
                                        while((count > 0)&&(sc < edsc))
                                        {
                                            if(*sc == ']')
                                                count--;
                                            sc++;
                                        }
                                        while(sc < edsc)
                                        {
                                            if(*sc != ' ')
                                                break;
                                            sc++;
                                        }
                                        if(chkop(sc,Fwd))
                                        {
                                            j = 3;
                                        }
                                        else{
                                            j = -1;
                                        }
                                    }
                                    else{
                                        //
                                        sc = sq->str -1;
                                        while(sc > s)
                                        {
                                            if(*sc != ' ')
                                                break;
                                            sc--;
                                        }
                                        if(chkop(sc,Bwd))
                                        {
                                            j = 3;
                                        }
                                        else{
                                            j = -1;
                                        }
                                    }
                                }
                            }
                        }
                        else{//data ref on the right
                            //
                            i = 0;
                            sc = s+1;
                            while(sc < sp->str)
                            {
                                if(*sc != ' ')
                                {
                                    i = 1;
                                    break;
                                }
                                sc++;
                            }
                            if(i == 0)
                            {
                                sc = sp->str + strlen(vars->dname);
                                count = vars->dim;
                                while((count > 0)&&(sc < edsc))
                                {
                                    if(*sc == ']')
                                        count--;
                                    sc++;
                                }
                                while(sc < edsc)
                                {
                                    if(*sc != ' ')
                                    {
                                        i = 1;
                                        break;
                                    }
                                    sc++;
                                }
                            }
                            if(i == 0)
                            {
                                j = 1;//atomic read;
                            }
                            else{
                                i = 0;
                                sq = sp->next;
                                while((sq != NULL)&&(sq->str < edsc))
                                {
                                    if(sq->len == sp->len)
                                    {
                                        i++;
                                        if(i == 2)
                                            break;
                                    }
                                    else{
                                        i = 3;
                                        break;
                                    }
                                    sq = sq->next;
                                }
                                if(i < 2)
                                {
                                    j = 4;
                                }
                                else{
                                    j = -1;
                                }
                            }
                        }//data ref on the left or right of "="
                    }//if "=" exists
                }//if is single statement

                if(j == 0)
                {
                    printf("Error in atomic check in DAC_calc %s, data %s!!\n",cal->cname,vars->dname);
                    exit(0);
                }
                //check merge
                i = 0;
                if(scq != NULL)
                {
                    sc = scq->str;
                    if(stsc - sc < 3)//adjacent
                    {
//                        printf("check atomic merge\n");
                        scluster = scp;
                        while(scluster->next != scq)
                            scluster = scluster->next;
                        if(j > 0)//atomic
                        {
                            if(scluster->len > 0)
                            {
                                if(scq->len == sp->len)
                                {
                                    //same data atomic
                                    if((j < 4)&&(scluster->len < 4))
                                    {
                                        //merge atomic capture
                                        i = 1;
                                        scluster->len = 5;
                                    }
                                    else{
                                        //merge critical
                                        i = 1;
                                        scluster->len = -1;
                                        scluster->type = 15;
                                    }
                                }
                            }
                            else{//last one critical
                                if(scq->len == sp->len)
                                {
                                    //merge critical
                                    i = 1;
                                }
                            }
                        }
                        else{//critical
                            if(scluster->len > 0)//last one is atomic
                            {
                                if(scq->len == sp->len)//same data
                                {
                                    //merge critical
                                    i = 1;
                                    scluster->len = -1;
                                    scluster->type = 15;
                                }
                            }
                            else{
                                //merge critical
                                i = 1;
                            }
                        }
                    }
                    if(i == 1)
                    {
//                        printf("merge atomic\n");
                        scq->str = edsc + 1;
                    }
                }
                if(i == 0)
                {
                    scluster = (struct strlist *)malloc(sizeof(struct strlist));
                    scluster->str = stsc;
                    scluster->len = j;//sign atomic type: read,write,update,capture,capture block
                    scluster->next = NULL;
                    if(scp == NULL)
                    {
                        scp = scluster;
                        scq = scp;
                    }
                    else{
                        scq->next = scluster;
                        scq = scluster;
                    }
                    //decide atomic type, atomic or critical
                    if(j > 0)
                    {
                        scluster->type = 14;
                    }
                    else{
                        scluster->type = 15;
                    }

                    scluster = (struct strlist *)malloc(sizeof(struct strlist));
                    scluster->str = edsc+1;
                    scluster->len = sp->len;
                    scluster->type = 32;
                    scluster->next = NULL;
                    scq->next = scluster;
                    scq = scluster;
                }

                //set sp
                sq = sp->next;
                while((sq != NULL)&&(sq->str < edsc))
                {
                    sq = sq->next;
                }
                sp = sq;
            }
            else{
                sp = sp->next;
            }
        }
        else{
            sp = sp->next;
        }
    }
    //merge atomic into sp
    if(scp != NULL)
    {
        sp = cal->strl;
        if(sp->str > scp->str)
        {
            cal->strl = scp;
            sq = scp;
            if((scp->type == 14)&&(scp->len > 0)&&(scp->len < 5))
            {
                scp = scp->next->next;
            }
            else{
                scp = scp->next;
            }
            sq->next = sp;
        }
        while((sp != NULL)&&(scp != NULL))
        {
            if(sp->str > scp->str)// change >= to >, atomic after data aggregation
            {
                sq->next = scp;
                sq = scp;
                if((scp->type == 14)&&(scp->len > 0)&&(scp->len < 5))
                {
//                    printf("here scp next next\n");
                    scp = scp->next->next;
                }
                else{
                    scp = scp->next;
                }
                sq->next = sp;
            }
            else{
                sq = sp;
                sp = sp->next;
            }
        }
        if((sp == NULL)&&(scp != NULL))
        {
            sq->next = scp;
        }
    }

//    printf("chkk4\n");
    //deal data reference
	cur = cal->start+1;
	end = cur + cal->clen;

	sp = cal->strl;
	if(sp->type == 10)
        sp = sp->next;
    if(sh->inum > 0)
    {
        idxn = sh->inum;
        spn = 0;
    }
    else{
        idxn = 0;
        spn = 1;
    }

	while(cur < end)
	{
//        printf("chkk6\n");
		if(sp != NULL)
		{
		//	printf("str=%d\n",sp->str);
			if(cur < sp->str)
			{
				size = sp->str - cur;
				if(fwrite(cur,size,1,fpkernel) != 1)
				{
					printf("failed to write dev file\n");
					exit(0);
				}
				/*printf("write\n");*/
				cur = sp->str;
			}
			if(sp->type < 14)
            {
                vp = sh->vars;
                vars = cal->vars;
                re = re0;
                for(i=1;i<sp->len;i++)      //the serial num start from 0 or 1?CHECK IT! => start from 1. vp initialization?
                {
                    vp = vp->next;
                    vars = vars->next;
                    re = re->next;
                }
            }
			switch(sp->type)
			{
			case 11:
//				printf("type11 %s\n",vars->dname);
                if(vp->dim > 0)
                {
                    cur = sp->str + strlen(vars->dname);
                    fprintf(fpkernel,"%s_h",vars->dname);

                    if((vars->dim > 0)&&(*cur != '['))//dim==0 || only name reference
                        break;
                    j = 0;
                    if((vp->sis == 1)&&(vp->ddname != NULL))//atom-dup
                    {
                        ids = (int *)malloc(sizeof(int)*(idxn+spn));
                        for(i=0;i<idxn+spn;i++)
                        {
                            if(re->idx & 1<<i)
                            {
                                ids[j] = i;
                                j++;
                            }
                        }
                    }
                    //if not, need to complete coordinate transformation
                    fputc('[',fpkernel);

                    k = vp->dim + j;
                    for(i=2;i<k;i++)      //should be dim-2
                        fputc('(',fpkernel);
                    if(j > 0)
                    {
                        for(i=0;i<j;i++)
                        {
                            if(i == 0)
                            {
                                if(idxn > 0)
                                    fprintf(fpkernel,"dac_i%d",ids[0]);
                                else
                                    fprintf(fpkernel,"dac_tn");
                            }
                            else{
                                if(i > 1)
                                    fputc(')',fpkernel);
                                //find range
                                //*range+dac_i%d
                                ip = sh->index;
                                while(ip != NULL)
                                {
                                    if(ip->id == ids[i])
                                        break;
                                    else
                                        ip = ip->next;
                                }
                                vq = sh->vars;
                                varsq = cal->vars;
                                while(vq != ip->ir->var)
                                {
                                    vq = vq->next;
                                    varsq = varsq->next;
                                }
                                fprintf(fpkernel,"*%s.range[%d]+dac_i%d",varsq->dname,ip->ir->range,ids[i]);
                            }
                        }
                        if(vp->dim > 0)
                        {
                            if(j > 1)
                                fputc(')',fpkernel);
                            fprintf(fpkernel,"*%s.range[0]",vars->dname);
                        }
                    }

                    rip = vp->ri;
                    for(i=j;i<k;i++)//ranges one by one
                    {
 //                       printf("range %d:",i);
                        if(i>0)
                            fputc('+',fpkernel);
                        if((rip != NULL)&&(i == rip->id))//indexed range
                        {
//                            printf("indexed\n");
                            if(rip->idl != NULL)//index
                            {
                                ip = rip->idl;
                                if(i < k-1)
                                {
                                    if(i == 0)
                                        fprintf(fpkernel,"dac_i%d*%s.range[%d]",ip->id,vars->dname,i+1);
                                    else
                                        fprintf(fpkernel,"dac_i%d)*%s.range[%d]",ip->id,vars->dname,i+1);
                                }
                                else{
                                    fprintf(fpkernel,"dac_i%d",ip->id);
                                }
                            }
                            else{//sp
                                spl = rip->spl;
                                if(*cur != '[')
                                {
                                    printf("unrecognized pattern %c in calc data ref\n",*cur);
                                    exit(0);
                                }
                                cur++;
                                fputc('(',fpkernel);
                                while(*cur != ']')
                                {
                                    fputc(*cur,fpkernel);
                                    cur++;
                                }
                                if(spl->id < idxn+spn)
                                {
                                    fprintf(fpkernel,"+ %s.range[%d]/dac_nts*dac_tn",vars->dname,i);// + offset
                                }
                                fputc(')',fpkernel);
                                if(i < k-1)
                                {
                                    if(i == 0)
                                        fprintf(fpkernel,"*%s.range[1]",vars->dname);
                                    else
                                        fprintf(fpkernel,")*%s.range[%d]",vars->dname,i+1);
                                }
                                cur++;
                            }
                            rip = rip->next;
                        }//if((rip != NULL)&&(i == rip->id))//indexed range
                        else{//not indexed
//                            printf("not indexed\n");
                            if(*cur != '[')
                            {
                                printf("unrecognized pattern %c in calc data ref\n",*cur);
                                exit(0);
                            }
                            cur++;
                            fputc('(',fpkernel);
                            while(*cur != ']')
                            {
                                fputc(*cur,fpkernel);
                                cur++;
                            }
                            fputc(')',fpkernel);
                            if(i < k-1)
                            {
                                if(i == 0)
                                    fprintf(fpkernel,"*%s.range[1]",vars->dname);
                                else
                                    fprintf(fpkernel,")*%s.range[%d]",vars->dname,i+1);
                            }
                            cur++;
                        }
                    }
                    fputc(']',fpkernel);
                }
                else{
                    fprintf(fpkernel,"*%s_h",vars->dname);
                    cur = sp->str + strlen(vars->dname);
                }
				break;
			case 12: //.type
//				printf("type12\n");
				/*for(i=0;i<10;i++)
					printf("%c",cur[i]);
				printf("\n");*/

				fprintf(fpkernel,"%s",re->ref->type);
				cur = sp->str + strlen(vars->dname) + 5; //5 = ".type"
				/*for(i=0;i<5;i++)
					printf("%c",cur[i]);
				printf("\n");*/
				break;
			case 13: //.range
//				printf("type13\n");
				count = 0;
                cur = sp->str + strlen(vars->dname) + 6;//name+.range
                if(*cur != '[')
                {
                    printf("Wrong pattern of .range in DAC_calc %s!!\n",cal->cname);
                    exit(0);
                }
                cur++;
                while(*cur != ']')
                {
                    if((*cur >= '0')&&(*cur <= '9'))
                    {
                        count *= 10;
                        count += *cur - '0';
                        cur++;
                    }
                    else{
                        printf("Wrong pattern of .range in DAC_calc %s!!\n",cal->cname);
                        exit(0);
                    }
                }
                cur++;
				if((vp->sis & 2)||(vp->sis & 4))
				{
				//	printf("%d\n",count);
					rip = vp->ri;
					for(i=0;i<vp->dim;i++)
					{
					//	printf("here\n");
						if((rip != NULL)&&(i == rip->id))
						{
							//printf("in here\n");
							if(rip->spl != NULL)
                            {
                                if(count == 0)//sp range
                                {
                                    if(rip->spl->id == 0)
                                    {
                                        fprintf(fpkernel,"(dac_tn == dac_nts-1)?");
                                        fprintf(fpkernel,"(%s.range[%d]-%s.range[%d]/dac_nts*dac_tn):",vars->dname,i,vars->dname,i);
                                        fprintf(fpkernel,"(%s.range[%d]/dac_nts)",vars->dname,i);
                                    }
                                    else{
                                        fprintf(fpkernel,"%s.range[%d]",vars->dname,i);
                                    }
                                    break;
                                }
                                else
                                    count--;
                            }
                            rip = rip->next;
						}
						else{
							if(count == 0)
                            {
                                fprintf(fpkernel,"%s.range[%d]",vars->dname,i);
                                break;
                            }
							else
								count--;
						}
					}
					//printf("range: %d\n",i);
				}
				else{
					fprintf(fpkernel,"%s.range[%d]",vars->dname,count);
				}
				break;
            case 14:
//                printf("type 14\n");
                fprintf(fpkernel,"\n#pragma omp atomic ");
                switch(sp->len)
                {
                case 1:
                    fprintf(fpkernel,"read\n");
                    break;
                case 2:
                    fprintf(fpkernel,"write\n");
                    break;
                case 3:
                    fprintf(fpkernel,"update\n");
                    break;
                case 4:
                    fprintf(fpkernel,"capture\n");
                    break;
                case 5:
                    fprintf(fpkernel,"capture\n{\n");
                    break;
                default:
                    break;
                }
                break;
            case 15:
                fprintf(fpkernel,"\n#pragma omp critical\n{\n");
                break;
            case 30:
                if(sp->len == 0)//mask == 0, no mask
                {
                    //do nothing
                   // printf("here2\n");
                    fprintf(fpkernel,"\n{\n");
                }
                else
                {
                    //printf("here3\n");
                    i = sp->len;
                    fprintf(fpkernel,"\nif");
                    count = 0;
                    j = 1;
                    while(i > 0)
                    {
                        if((sp->len & j) > 0)
                        {
                            count++;
                            if(count > 1)
                                break;
                        }
                        j <<= 1;
                        i >>= 1;
                    }
                    if(count > 1)
                        fputc('(',fpkernel);
                    j = 1;
                    count = 0;
                    i = scluster->len;
                    k = 0;
                    while(i > 0)
                    {
                        if((sp->len & j) > 0)
                        {
                            if(count > 0)
                                fprintf(fpkernel,"&&");
                            if(idxn > 0)
                                fprintf(fpkernel,"(dac_i%d == 0)",k);
                            else
                                fprintf(fpkernel,"(dac_tn == 0)");
                            count++;
                        }
                        j <<= 1;
                        i >>= 1;
                        k++;
                    }
                    if(count > 1)
                        fputc(')',fpkernel);
                    fprintf(fpkernel,"{\n");
                }
                break;
            case 32:
//                printf("type 32\n");
                fprintf(fpkernel,"\n}\n");
                break;
			default:
				printf("Wrong str type in calc\n");
				break;
			}//switch(sp->type)
//			printf("chkk7\n");
			while((sp != NULL)&&(cur >= sp->str))
            {
                sp = sp->next;
                //printf("cur: %s, next:%s\n",cur,sp->str);
            }
		}//if(sp!=NULL)
		else//copy the rest
        {
			size = end - cur;
			if(fwrite(cur,size,1,fpkernel) != 1)
			{
				printf("failed to write dev file\n");
				exit(0);
			}
			break;
		}
	}
	return ker->count;
}
//check dedup,if not create related kernel create it
int chkdedup(struct reflist *re,struct varlist *var0,struct shelllist *sh)//return dedup type, 1:reduction;2:merge;0:NULL and Error;
{
	struct strlist *sp;
	struct kernellist *ddk,*k0;
	struct typelist *tp;
	struct calclist *ddp;
	struct datalist *var;
//	struct splist *spl;
//	struct indexlist *idx;
	struct rangei *rgi;
	size_t size;
	int count,i,j,k,l,idcount,atomdup;
	char *cur,*end;
	int *ids,*rgids;
//	printf("checking...");
	ddp = calcL;
	//check and find the name in dedup list
	while(ddp != NULL)
	{
	//	printf("here...%s...",ddp->dname);
		if(strcmp(var0->ddname,ddp->cname) == 0)
		{
			break;
		}
	//	printf("next...");
		ddp = ddp->next;
	}
	if(ddp == NULL)
	{
		printf("no match of dedup name \"%s\" !!\n",var0->ddname);
		exit(0);
	}
//	printf("get dedup name...");
	ddk = ddp->ker;
	k0 = ddp->ker;
	//check data type
	count = 1;//0 or 1? kernel count start from 1;
	while(ddk != NULL)
	{
//		printf("here...");
		if(strcmp(ddk->sname,sh->shname) == 0)
		{
		    tp = ddk->t;
		    if((tp != NULL)&&(strcmp(re->ref->type,tp->type) == 0))
            {
                re->ddi->ddc = count;
                switch(ddp->datan)
                {
                case 2:
                    return 1;
                    break; //233333
                case 3:
                    return 2;
                    break;
                default:
                    return 0;
                    break;
                }
            }
		}
		k0 = ddk;
		ddk = ddk->next;
		count++;
	}
	if(ddp->datan == 2)
        i = 1;
    else if(ddp->datan == 3)
        i = 2;
    else
        return 0;
    if((var0->sis == 1)&&(i == 2))
    {
        printf("Conflict dedup type and data attributes!!\n");
        return 0;
    }
	ddk = (struct kernellist *)malloc(sizeof(struct kernellist));
	tp = (struct typelist *)malloc(sizeof(struct typelist));
	tp->next = NULL;
	tp->type = re->ref->type;
	ddk->count = count;
	ddk->next = NULL;
	ddk->sname = sh->shname;
	ddk->t = tp;
	//record dedup kernel count
	re->ddi->ddc = count;
	if(k0 == NULL)
		ddp->ker = ddk;
	else
		k0->next = ddk;
//	printf("dedup ker...");

	//kernel declaration
	var = ddp->vars;
	fprintf(fpkernel,"\nvoid %s%d(",var0->ddname,count);
	fprintf(fpdacrt,"\nvoid %s%d(",var0->ddname,count);
	//sign dedup type
	count = i;
	fprintf(fpkernel,"%s *%s_h, struct dss %s",tp->type,var->dname,var->dname);
	fprintf(fpdacrt,"%s *, struct dss",tp->type);

	idcount = 0;
	j = re->idx;
	while(j > 0)
    {
        if(j & 1)
        {
            fprintf(fpkernel,", int r_%d",idcount);
            fprintf(fpdacrt,", int");
            idcount++;
        }
        j >>= 1;
    }
    if(var0->sis == 1)//atom-dup
    {
        fprintf(fpkernel,", %s *%s_dUp)\n{\n",tp->type,var->dname);
        fprintf(fpdacrt,", %s *);\n",tp->type);
    }
    else if(count == 2)
    {
        fprintf(fpkernel,", %s *%s_bUff)\n{\n",tp->type,var->dname);
        fprintf(fpdacrt,", %s *);\n",tp->type);
    }
    else{
        fprintf(fpkernel,")\n{\n");
    }

    ids = (int *)malloc(sizeof(int)*idcount);
    j = re->idx;
    i = 0;
    idcount = 0;
    atomdup = 0;
    while(j > 0)
    {
        if(j & 1)
        {
            ids[idcount] = i;
            idcount++;
        }
        i++;
        j >>= 1;
    }

	if(var0->sis == 1)//atom-dup
    {
        atomdup = idcount;
        k = atomdup + var0->dim;
        rgids = (int *)malloc(sizeof(int)*k);
        for(i=0;i<idcount;i++)
            rgids[i] = ids[i];
        while(i < k)
        {
            rgids[i] = -1;
            i++;
        }
    }
    else{// non-atomic data
        k = var0->dim;
        rgids = (int *)malloc(sizeof(int)*k);
        for(i=0;i<k;i++)
            rgids[i] = -1;
        rgi = var0->ri;
        while(rgi != NULL)
        {
            if(rgi->idl != NULL)
            {
                j = rgi->id;
                rgids[j] = rgi->idl->id;
            }
            else if(rgi->spl != NULL)
            {
                j = rgi->id;
                if(rgi->spl->id == 0)//only the selected sp matter, aka the fires sp in omp
                {
                    rgids[j] = rgi->spl->id;
                }
            }
            rgi = rgi->next;
        }
    }

    fprintf(fpkernel,"\tint tID_0, STP_0");
    for(i=0;i<idcount;i++)
    {
        fprintf(fpkernel,", tID_%d",i);
    }
    fprintf(fpkernel,";\n");
//    for(j=0;i<var0->dim;i++)
//    {
//        if(rgids[j] == ids[i])
//        {
//            fprintf(fpkernel,"\tint STP_%d = r_%d/N_sp%d;\n",i,j,i);
//            break;
//        }
//    }

    if(count == 2)
    {
        fprintf(fpkernel,"\tint sTrobe = 0;\n");
        fprintf(fpkernel,"\tmemcpy(%s_bUff,%s_h,%s.size);\n",var->dname,var->dname,var->dname);
    }
    //generate dedup process
    for(i=idcount-1;i>=0;i--)
    {
        if(i == 0)// last round, or sp
        {
            for(l=0;l<k;l++)
            {
                if(rgids[l] == ids[0])
                    break;
            }
            //some thing?
            if((l < atomdup)||(sh->inum > 0))//check index or sp
            {
                //
                fprintf(fpkernel,"for(STP_0 = 1;STP_0 < r_0;STP_0 <<= 1){\n");
            }
            else{//sp
                //
                fprintf(fpkernel,"for(STP_0 = %s.range[%d]/r_0;STP_0 < %s.range[%d];STP_0 <<= 1){\n",var->dname,l,var->dname,l);
            }
            fprintf(fpkernel,"#pragma omp parallel for private(tID_0) firstprivate(STP_0)\n");

            if(l < atomdup)
            {
                fprintf(fpkernel,"for(tID_0 = 0;tID_0+STP_0 < r_0;tID_0 += STP_0*2){\n");
            }
            else{
                fprintf(fpkernel,"for(tID_0 = 0;tID_0+STP_0 < %s.range[%d];tID_0 += STP_0*2){\n",var->dname,l);
            }

        }
        else{
            //
            fprintf(fpkernel,"#pragma omp parallel for");
            if(i > 2)
                fprintf(fpkernel," collapse(%d)",i);
            fprintf(fpkernel," private(tID_0");
            for(l=1;l<=i;l++)
                fprintf(fpkernel,",tID_%d",l);
            fputc(')',fpkernel);
            for(l=0;l<i;l++)
            {
                fprintf(fpkernel,"for(tID_%d = 0;tID_%d < r_%d;tID_%d++){\n",l,l,l,l);
                for(j=0;j<=l;j++)
                    fputc('\t',fphost);
            }
            fprintf(fpkernel,"for(tID_%d = 1;tID_%d < r_%d;tID_%d++){\n",i,i,i,i);
        }

        cur = ddp->start + 1;
        end = cur + ddp->clen - 2;//*end is "}"-1
        sp = ddp->strl;
        while(cur < end)
        {
            //printf("here...");
            if(sp != NULL)
            {
                if(cur < sp->str)
                {
//                    printf("copy...");
                    size = sp->str - cur;
                    if(fwrite(cur,size,1,fpkernel) != 1)
                    {
                        printf("failed to write dev file\n");
                        exit(0);
                    }
                    cur = sp->str;
                }
                var = ddp->vars;
                for(l=1;l<sp->len;l++) //sp->len indicates the order of variables
                    var = var->next;
                size = strlen(var->dname);
                k = var0->dim + atomdup;
                switch(sp->type)
                {
                case 11:  //ref
//                    printf("11...");
                    if(count == 2)//merge
                    {
                        fprintf(fpkernel,"%s_bUff",ddp->vars->dname);
                    }
                    else if(atomdup > 0)
                    {
                        fprintf(fpkernel,"%s_dUp",ddp->vars->dname);
                    }
                    else{
                        fprintf(fpkernel,"%s_h",ddp->vars->dname);
                    }

                    if((re->ref->dim > 0)||(atomdup > 0))//maybe no exception??
                    {
                        fputc('[',fpkernel);
                    }
                    cur = sp->str + size;
                    for(l=1;l<k;l++)      //should be dim-1,protect 0~n-1
                        fputc('(',fpkernel);
                    if(count == 2)
                    {
                        if(sp->len < 3)
                        {
                            fprintf(fpkernel,"sTrobe*%s.range[0]+",ddp->vars->dname);//strobe or 1-strobe?
                        }
                        else{
                            fprintf(fpkernel,"(1-sTrobe)*%s.range[0]+",ddp->vars->dname);
                        }
                    }
                    for(l=0;l<k;i++)//ranges one by one
                    {
                        if(l > 0)
                        {
                            if(l < atomdup)
                            {
                                fprintf(fpkernel,"*r_%d+",l);
                            }
                            else{
                                fprintf(fpkernel,"*%s.range[%d]+",ddp->vars->dname,l-atomdup);
                            }
                        }

                        if(rgids[l] >= 0)//indexed
                        {
                            for(j=0;i<idcount;j++)
                            {
                                if(rgids[l] == ids[j])
                                    break;
                            }
                            if(j > i)//dedup done ranges
                            {
                                fputc('0',fpkernel);
                            }
                            else if(j == i){//current dedup range
                                if(j > 0)
                                {
                                    if(sp->len == 2)
                                        fprintf(fpkernel,"tID_%d",j);
                                    else
                                        fputc('0',fpkernel);
                                }
                                else{
                                    //last ids
                                    fprintf(fpkernel,"tID_0");
                                    if(sp->len == 2)
                                    {
                                        fprintf(fpkernel,"+STP_0");
                                    }
                                }
                            }
                            else{//j < i
                                //
                                fprintf(fpkernel,"tID_%d",j);
                            }
                            if((atomdup == 0)&&(ids[j] >= sh->inum))
                            {
                                fputc('+',fpkernel);
                                if(*cur != '[')
                                {
                                    printf("unrecognized pattern in dedup data ref:%c vs [.\n",*cur);
                                    exit(0);
                                }
                                cur++;
                                while(*cur != ']')
                                {
                                    fputc(*cur,fpkernel);
                                    cur++;
                                }
                                cur++;
                            }
                        }
                        else{//not indexed
                            if(*cur != '[')
                            {
                                printf("unrecognized pattern in dedup data ref:%c vs [.\n",*cur);
                                exit(0);
                            }
                            cur++;
                            while(*cur != ']')
                            {
                                fputc(*cur,fpkernel);
                                cur++;
                            }
                            cur++;
                        }
                        if(l < k-1)
                            fputc(')',fpkernel);
                    }
                    fputc(']',fpkernel);
                    break;
                case 12:  //type
//                    printf("12...");
                    fprintf(fpkernel,"%s",re->ref->type);
                    cur = sp->str + size + 5; //5 = ".type" size is the strlen of original var
                    break;
                case 13:  //range
//                    printf("13...");
                    cur = sp->str + size + 2;
                    j = 0;
                    while((*cur >= '0')&&(*cur <= '9'))
                    {
                        j *= 10;
                        j += *cur - '0';
                        cur++;
                    }
                    for(l=0;l<k;l++)
                    {
                        if((rgids[l] < 0)||((rgids[l] >= sh->inum)&&(atomdup == 0)))
                        {
                            if(j == 0)
                                break;
                            else
                                j--;
                        }
                    }
                    if(rgids[l] < 0)
                    {
                        fprintf(fpkernel,"%s.range[%d]",ddp->vars->dname,l);
                    }
                    else{
                        for(j=0;j<idcount;j++)
                        {
                            if(ids[j] == rgids[l])
                                break;
                        }
                        if(sp->len == 2)
                        {
                            fprintf(fpkernel,"(tID%d+STP_%d+STP_%d < %s.range[%d] ? STP_%d : %s.range[%d]-tID%d-STP_%d)",j,j,j,ddp->vars->dname,l,j,ddp->vars->dname,l,j,j);
                        }
                        else{
                            fprintf(fpkernel,"STP_%d",j);
                        }
                    }
                    break;
                default:
                    printf("%d...",sp->type);
                    break;
                }
                sp = sp->next;
            }
            else{
//                printf("last...");//the last } well enclose the fprintf("if(") inside fprintf("while(") --->no more
                size = end - cur;
                if(fwrite(cur,size,1,fpkernel) != 1)
                {
                    printf("failed to write dev file\n");
                    exit(0);
                }
                break;
            }
        }//while(cur < end)
        //
        if(i > 0)
        {
            for(l=i-1;l>=0;l--)
            {
                for(j=0;j<=l;j++)
                    fputc('\t',fphost);
                fprintf(fpkernel,"}\n");
            }
            fprintf(fpkernel,"}\n");
            fprintf(fpkernel,"#pragma omp barrier\n");
            if(count == 2)
            {
                fprintf(fpkernel,"\tstrobe = 1 - strobe;\n");
            }
        }
        else{
            fprintf(fpkernel,"\t}\n");
            if(count == 2)
            {
                fprintf(fpkernel,"\tstrobe = 1 - strobe;\n");
            }
            fprintf(fpkernel,"}\n");
        }
        //fprintf(fpkernel,"\t}\n");//end of generated while, synchronization in every loop
    }
    if(count == 2)
    {
        fprintf(fpkernel,"\tmemcpy(%s_h,%s_buff+sTrobe*%s.size,%s.size);\n",ddp->vars->dname,ddp->vars->dname,ddp->vars->dname,ddp->vars->dname);
    }
	fprintf(fpkernel,"}\n");
	return count;
}

void dealcomptOMP(struct strlist *p)
{
	struct reflist *re0,*re,*retail;//record data objects
	struct shelllist *sh;
	struct calclist *cal;
	struct datalist *d;
	struct arglist *ap0,*ap,*aptail;
	//struct posprop *pp;
	struct varlist *var0,*var1;
	struct indexlist *idl;
	struct splist *spl;
	struct rangei *ri;
//	struct irangelist *irl;
	//struct dataref *drf;
	char *var;
	char *c;
	int i,j,len,s,datan,argn,kc,ddflag;//kc : kernel count,dc: dedup count
	printf("dealcompt start...");
//	comptcount--; ==>no longer needed
	c = p->str;
//search data variables
	i = 0;
	len = 0;
	re0 = NULL;
	datan = 0;
	argn = 0;
	ddflag = 0;
	while(c[i] != '<')
	{
		i++;
	}
	i++;
//	printf("record data\n");
	while(1)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in compt expression, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in compt expression, can not begin with digits\n");
				exit(0);
			}
			else{
				len++;
			}
		}
		else if((c[i] == '_')||((c[i] > 64)&&(c[i] < 91))||((c[i] > 96)&&(c[i] < 123)))
		{
			len++;
		}
		else if((c[i] == ',')||(c[i] == '>'))
		{
			datan++;
			var = (char *)malloc(sizeof(char) * (len+1));
			s = i-len;
			for(j=0;j<len;j++)
			{
				var[j] = c[s];
				s++;
			}
			var[j] = '\0';
			d = dataL;
			while(d != NULL)
			{
				if(strcmp(d->dname,var) == 0)
				{
					re = (struct reflist *)malloc(sizeof(struct reflist));
					re->ref = d;
					re->ddi = NULL;
					re->next = NULL;
					//re->tid = 0;
					re->idx = 0;
					re->mask = 0;
					if(re0 == NULL)
					{
						re0 = re;
						retail = re;
					}
					else{
						retail->next = re;
						retail = re;
					}
					break;
				}
				d = d->next;
			}
			if(d == NULL)
			{
				printf("unrecognized data reference in compt expression\n");
				exit(0);
			}
			if(c[i] == ',')
				len = 0;
			else
				break;
		}
		else{
			printf("illegal char detected in compt expression\n");
			exit(0);
		}
		i++;
	}
//record shell name
//	printf("record shell name\n");
	i++;
	len = 0;
	while(c[i] != '>')
		i++;
	i++;
	while(1)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in compt expression, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in compt expression, can not begin with digits\n");
				exit(0);
			}
			else{
				len++;
			}
		}
		else if((c[i] == '_')||((c[i] > 64)&&(c[i] < 91))||((c[i] > 96)&&(c[i] < 123)))
		{
			len++;
		}
		else if(c[i] == '(')
		{
			var = (char *)malloc(sizeof(char) * (len+1));
			s = i-len;
			for(j=0;j<len;j++)
			{
				var[j] = c[s];
				s++;
			}
			var[j] = '\0';
			sh = shellL;
			while(sh != NULL)
			{
				if(strcmp(sh->shname,var) == 0)
				{
					break;
				}
				sh = sh->next;
			}
			if(sh == NULL)
			{
				printf("unrecognized data reference in compt expression\n");
				exit(0);
			}
			break;
		}
		else{
			printf("illegal char '%c' detected in compt expression\n",c[i]);
			exit(0);
		}
		i++;
	}
//record calc name
//	printf("record calc name\n");
	i++;
	len = 0;
	while(1)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in compt expression, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in compt expression, can not begin with digits\n");
				exit(0);
			}
			else{
				len++;
			}
		}
		else if((c[i] == '_')||((c[i] > 64)&&(c[i] < 91))||((c[i] > 96)&&(c[i] < 123)))
		{
			len++;
		}
		else if((c[i] == '(')||(c[i] == ')'))      //consider arguments, should be '('?? ==> already corrected from ')' to '('
		{
			var = (char *)malloc(sizeof(char) * (len+1));
			s = i-len;
			for(j=0;j<len;j++)
			{
				var[j] = c[s];
				s++;
			}
			var[j] = '\0';
			cal = calcL;
			while(cal != NULL)
			{
//				printf("%s\n",cal->cname);
//				printf("calc name:%s\n",cal->cname);
				if(strcmp(cal->cname,var) == 0)
				{
					break;
				}
				cal = cal->next;
			}
			if(cal == NULL)
			{
				printf("unrecognized calc reference \"%s\" in compt expression\n",var);
				exit(0);
			}
			break;
		}
		else{
			printf("illegal char detected in compt expression\n");
			exit(0);
		}
		i++;
	}
	//record calc args
	i++;
	s = i;
	j = 0;
	ap0 = NULL;
	while(i < p->len)
	{
		if(c[i] == ',')
		{
			argn++;
			len = i-s;   //whether need to check len>0 ?
			ap = (struct arglist *)malloc(sizeof(struct arglist));
			ap->nmortp = (char *)malloc(sizeof(char)*(len+1));
			ap->next = NULL;
			len = 0;
			while(s < i)
			{
				ap->nmortp[len] = c[s];
				s++;
				len++;
			}
			ap->nmortp[len] = '\0';
			s++;
			if(ap0 == NULL)
			{
				ap0 = ap;
				aptail = ap;
			}
			else{
				aptail->next = ap;
				aptail = ap;
			}
		}
		else if(c[i] == '(')
			j--;
		else if(c[i] == ')')
		{
			j++;
			if(j > 0)
			{
				argn++;
				len = i-s;
				ap = (struct arglist *)malloc(sizeof(struct arglist));
				ap->nmortp = (char *)malloc(sizeof(char)*(len+1));
				ap->next = NULL;
				len = 0;
				while(s < i)
				{
					ap->nmortp[len] = c[s];
					s++;
					len++;
				}
				ap->nmortp[len] = '\0';
				if(ap0 == NULL)
				{
					ap0 = ap;
					aptail = ap;
				}
				else{
					aptail->next = ap;
					aptail = ap;
				}
				break;
			}
		}
		i++;
	}
//	printf("record args ends\n");
	//first we need to check that whether the num of data objects and args match that in shell and calc definition.
	if((datan != sh->dnum)||(datan != cal->datan))
	{
		printf("data num not match in DAC computation!");
		exit(0);
	}
	if(argn != cal->argn)
	{
		printf("arg num not match in DAC computation!");
		exit(0);
	}

//    len = 1;
//    len <<= j;
//    len--;//up bound

    re = re0;
	var0 = sh->vars;
	if(sh->inum > 0)
    {
        j = sh->inum;
    }
    else{
        j = 1;
    }
	while(re != NULL)
	{
		re->iod = var0->io;
		//count re->mask
		idl = sh->index;
		spl = sh->sp;
		len = 0;
		while(len < j)
        {
            if(idl != NULL)
            {
                ddflag = idl->iol;
                idl = idl->next;
            }
            else if(spl != NULL)
            {
                ddflag = spl->iol;
                spl = spl->next;
            }
            else{
                //just in case
                break;
            }
            if((var0->ioid & ddflag) == 0) //using ddflag as tmp
                re->mask |= 1<<len;//mask : chosen identifiers that not presented -->a little problem: if identifiers exceed 32, re->mask will go wrong
            len++;
        }
		//count idx
		if((var0->sis == 1)&&(var0->ddname != NULL))
        {
            //atomic-dup
            len = 1;
            len <<= j;
            len--;
            re->idx = re->mask ^ len;//xor
            //dup data here--->leave runtime in runtime
            //
        }
        if(var0->sis > 1)
        {
            ri = var0->ri;
            while(ri != NULL)
            {
                if(ri->id < j)
                {
                    re->idx |= 1<<ri->id;
                }
                ri = ri->next;
            }
        }
        if((var0->sis > 0)&&(var0->ddname != NULL))
        {
            if(re->idx > 0)//check this!!! if identifiers not selected, then no dup and dedup
            {
                re->ddi = (struct dd *)malloc(sizeof(struct dd));
                re->ddi->ddname = var0->ddname;
                re->ddi->ddflag = chkdedup(re,var0,sh);
                if(re->ddi->ddflag == 0)
                {
                    printf("chkdedup failed!\n");
                    exit(0);
                }
                if(var0->sis == 1)
                {
                    //atom-dup
                    fprintf(fphost,"%s *%s_dup;\n",re->ref->type,re->ref->dname);
                    if(sh->index != NULL)// memory allocation for _dup
                    {
                        fprintf(fphost,"%s_dup = (%s *)malloc(",re->ref->dname,re->ref->type);
                        //for
                        idl = sh->index;
                        len = re->idx;
                        while(len > 0)
                        {
                            if(len & 1)
                            {
                                retail = re0;
                                var1 = sh->vars;
                                while(var1 != idl->ir->var)
                                {
                                    var1 = var1->next;
                                    retail = retail->next;
                                }
                                fprintf(fphost,"%s.range[%d]*",retail->ref->dname,idl->ir->range);
                            }
                            len >>= 1;
                            idl = idl->next;
                        }
                        fprintf(fphost,"%s.size);\n",re->ref->dname);
                    }
                }
                if(re->ddi->ddflag == 2)
                {
                    //merge-dup
                    fprintf(fphost,"%s *%s_buff;\n",re->ref->type,re->ref->dname);
                    fprintf(fphost,"%s_buff = (%s *)malloc(%s.size*2);\n",re->ref->dname,re->ref->type,re->ref->dname);
                }
            }
        }
		re = re->next;
		var0 = var0->next;
	}

	printf("chkkernel...");
	kc = chkkernel(re0,cal,sh);
//	printf("chkkernel end\n");

	//data dup-->two types: atom-dup and merge-dup

	if(sh->inum == 0)
    {
        //deal sp
        re = re0;
        var0 = sh->vars;
        while(re != NULL)
        {
            if((var0->sis == 1)&&(re->ddi != NULL))
            {
                fprintf(fphost,"\t%s_dup = (%s *)malloc(dac_nts*%s.size);\n}\n",re->ref->dname,re->ref->type,re->ref->dname);
                //initialize _dup
                fprintf(fphost,"\tmemcpy(%s_dup+dac_nt*%s.size,%s_h,%s.size);\n",re->ref->dname,re->ref->dname,re->ref->dname,re->ref->dname);
            }
            re = re->next;
            var0 = var0->next;
        }
        fprintf(fphost,"#pragma omp parallel default(shared)");
        ap = ap0;
        if(ap != NULL)
        {
            fprintf(fphost," firstprivate(%s",ap->nmortp);
            ap = ap->next;
            while(ap != NULL)
            {
                fprintf(fphost,",%s",ap->nmortp);
                ap = ap->next;
            }
            fputc(')',fphost);
        }
        fprintf(fphost," private(dac_tn)\n{\n%s",dealsp1);

        fprintf(fphost,"\t%s%d(",cal->cname,kc);
        ap = ap0;
        while(ap != NULL)//normal args
        {
            fprintf(fphost,"%s,",ap->nmortp);
            ap = ap->next;
        }
        re = re0;
        var0 = sh->vars;
        while(re != NULL)
        {
            if((var0->sis == 1)&&(re->ddi != NULL))
                fprintf(fphost,"%s_dup,%s,",re->ref->dname,re->ref->dname);
            else
                fprintf(fphost,"%s_h,%s,",re->ref->dname,re->ref->dname);
            re = re->next;
            var0 = var0->next;
        }
        fprintf(fphost,"dac_tn,dac_nts);\n}\n");//last two arg should be dac_nts,dac_tn;
    }
    else{
        //deal index
        fprintf(fphost,"#pragma omp parallel for");
        if(sh->inum > 1)
        {
            fprintf(fphost," collapse(%d)",sh->inum);
        }
        fprintf(fphost," private(dac_i0");
        for(j=1;j<sh->inum;j++)
        {
            fprintf(fphost,",dac_i%d",j);
        }
        fputc(')',fphost);

        if(argn != 0)
        {
            ap = ap0;
            fprintf(fphost," firstprivate(%s",ap->nmortp);
            ap = ap->next;
            while(ap != NULL)
            {
                fprintf(fphost,",%s",ap->nmortp);
                ap = ap->next;
            }
            fputc(')',fphost);
        }
        fputc('\n',fphost);

        idl = sh->index;
        for(j=0;j<sh->inum;j++)
        {
            var0 = idl->ir->var;
            re = re0;
            for(i=0;i<var0->vid;i++)//check i if for other use
                re = re->next;
            fprintf(fphost,"for(dac_i%d=0;dac_i%d<%s.range[%d];dac_i%d++){\n",j,j,re->ref->dname,idl->ir->range,j);
            for(i=0;i<=j;i++)
                fputc('\t',fphost);
            idl = idl->next;
        }
        fprintf(fphost,"%s%d(",cal->cname,kc);//calc name
        ap = ap0;
        while(ap != NULL)//normal args
        {
            fprintf(fphost,"%s,",ap->nmortp);
            ap = ap->next;
        }
        re = re0;
        var0 = sh->vars;
        while(re != NULL)
        {
            if((var0->sis == 1)&&(re->ddi != NULL))
                fprintf(fphost,"%s_dup,%s,",re->ref->dname,re->ref->dname);
            else
                fprintf(fphost,"%s_h,%s,",re->ref->dname,re->ref->dname);
            re = re->next;
            var0 = var0->next;
        }
        fprintf(fphost,"dac_i0");
        for(j=1;j<sh->inum;j++)//control
        {
            fprintf(fphost,",dac_i%d",j);
        }
        fprintf(fphost,");\n");
        for(j=0;j<sh->inum;j++)
        {
            for(i=sh->inum-j-1;i>0;i--)
                fputc('\t',fphost);
            fprintf(fphost,"}\n");
        }
    }

    //deal dedup
    //dedup(data,start1,start2,end,range?)
    re = re0;
    var0 = sh->vars;
    while(re != NULL)
    {
        if(re->ddi != NULL)
        {
            //call dedup
            //for idx
            fprintf(fphost,"\t%s%d(%s_h,%s",re->ddi->ddname,re->ddi->ddc,re->ref->dname,re->ref->dname);
            if(sh->inum > 0)
            {
                //for idx
                idl = sh->index;
                len = re->idx;
                while(len > 0)
                {
                    if(len & 1)
                    {
                        retail = re0;
                        var1 = sh->vars;
                        while(var1 != idl->ir->var)
                        {
                            var1 = var1->next;
                            retail = retail->next;
                        }
                        fprintf(fphost,",%s.range[%d]",retail->ref->dname,idl->ir->range);
                    }
                    len >>= 1;
                    idl = idl->next;
                }
            }
            else{
                fprintf(fphost,",dac_nts");
            }

            if(re->ddi->ddflag == 2)
            {
                fprintf(fphost,",%s_buff);\n",re->ref->dname);
                //free _buff
                fprintf(fphost,"\tfree(%s_buff);\n",re->ref->dname);
            }
            else if(var0->sis == 1)
            {
                fprintf(fphost,",%s_dup);\n",re->ref->dname);
                //data copy
                //fprintf(fphost,"\tmemcpy(%s_h,%s_dup,%s.size);\n",re->ref->dname,re->ref->dname,re->ref->dname);-->conceal in dedup kernel
                //free _dup
                fprintf(fphost,"\tfree(%s_dup);\n",re->ref->dname);
            }
            else
            {
                fprintf(fphost,");\n");
            }
        }
        re = re->next;
        var0 = var0->next;
    }

	free(re0);
}

int dealshapeOMP(struct shapelist *p)
{
	struct datalist *d;
	int i,f;
	d = p->data;

	if(d->type == NULL)//first define;
	{
		d->type = p->type;
		fprintf(fphost,"%s *%s_h;\n",d->type,d->dname);
		fprintf(fphost,"\t%s_h = NULL;\n",d->dname);
//		fprintf(fphost,"\t%s.hp = NULL;\n",d->dname);
//		fprintf(fphost,"\t%s.ts = 0;\n",d->dname);
//		fprintf(fphost,"\t%s.dp = NULL;\n",d->dname);
		fprintf(fphost,"\t%s.type = sizeof(%s);\n",d->dname,d->type);
        if((p->dim_s > 0)&&(d->dim > p->dim_s))//check data dimension
        {
            printf("Unmatched dimension of data %s, %d in declaration, %d in DAC_shape\n",d->dname,d->dim,p->dim_s);
            exit(0);
        }
		if((d->dim == 0)&&(p->dim_s > 0))
			d->dim = p->dim_s;
		fprintf(fphost,"\t%s.dim = %d;\n",d->dname,d->dim);
		if(d->dim > 0)
		{
			fprintf(fphost,"\t%s.range = (size_t *)malloc(sizeof(size_t)*%d);\n",d->dname,d->dim);
			fprintf(fphost,"\t%s.range_u = (size_t *)malloc(sizeof(size_t)*%d);\n",d->dname,d->dim);
		}
		else{
			fprintf(fphost,"\t%s.range = (size_t *)malloc(sizeof(size_t));\n",d->dname);
			fprintf(fphost,"\t%s.range[0] = 1;\n",d->dname);
			fprintf(fphost,"\t%s.range_u = NULL;\n",d->dname);
		}
		if((d->range == NULL)&&(p->range_s != NULL))
		{
			d->range = p->range_s;
		}
		f = 0;
		for(i=0;i<d->dim;i++)
		{
			if(d->range[i] != NULL)//data declaration superior
			{
				fprintf(fphost,"\t%s.range_u[%d] = %s;\n",d->dname,i,d->range[i]);
				fprintf(fphost,"\t%s.range[%d] = %s;\n",d->dname,i,d->range[i]);
			}
			else if((p->range_s != NULL)&&(p->range_s[i] != NULL))
            {
                d->range[i] = p->range_s[i];
				fprintf(fphost,"\t%s.range_u[%d] = %s;\n",d->dname,i,p->range_s[i]);
				fprintf(fphost,"\t%s.range[%d] = %s;\n",d->dname,i,p->range_s[i]);
			}
			else{
               // fprintf(fphost,"%s.range_u[%d] = -1;\n",d->dname,i);
                f = 1;//undefined range
			}
		}
		if(f == 0)
        {
            fprintf(fphost,"\t%s.size = %s.type",d->dname,d->dname);
        //	printf("size\n");
            if(d->dim > 0)
            {
                for(i=0;i<d->dim;i++)
                {
                    fprintf(fphost,"*%s.range[%d]",d->dname,i);
                }
            }
            fprintf(fphost,";\n");
            fprintf(fphost,"\tif(%s_h == NULL){\n",d->dname);
            fprintf(fphost,"\t\t%s_h = (%s *)malloc(%s.size);\n\t}\n",d->dname,d->type,d->dname); //size is the size in memory = type*item_num
           // fprintf(fphost,"\t\t%s.dp = NULL\n}\n",d->dname);
//            fprintf(fphost,"\tif(%s.hp == NULL){\n",d->dname);
//            fprintf(fphost,"\t\t%s.hp = (void *)%s_h;\n\t}\n",d->dname,d->dname);
        }

	}
	else{//dac_shape try to change data attributes
		if(strcmp(d->type, p->type) != 0)
		{
			printf("DAC_shape can not change the data type of %s\n",d->dname);
			exit(0);
		}
		if(d->dim > p->dim_s)
        {
            printf("DAC_shape can not decrease data dimension of %s\n",d->dname);// DAC_shape can?
            exit(0);
        }

        if(d->dim > 0)
        {
            f = 0;
            for(i=0;i<d->dim;i++)
            {
                if(d->range[i] == NULL)
                {
                    if(p->range_s[i] != NULL)
                    {
                        d->range[i] = p->range_s[i];
                        fprintf(fphost,"\t%s.range_u[%d] = %s;\n",d->dname,i,p->range_s[i]);
                        fprintf(fphost,"\t%s.range[%d] = %s;\n",d->dname,i,p->range_s[i]);
                    }
                    else{
                        f = 1;
                    }
                }
                else{
                    if(p->range_s[i] != NULL)
                    {
                        fprintf(fphost,"\tif(%s <= %s.range_u[%d]){\n",p->range_s[i],d->dname,i);
                        fprintf(fphost,"\t\t%s.range[%d] = %s;}\n",d->dname,i,p->range_s[i]);
                        fprintf(fphost,"\telse{\n");
                        fprintf(fphost,"\t\tprintf(\"Can not assign a larger range than the original range in DAC_shape!!\");\n");
                        fprintf(fphost,"\t\texit(0);\n\t}\n");
                    }

                }
            }
            if(f == 0)
            {
                fprintf(fphost,"\t%s.size = %s.type",d->dname,d->dname);
            //	printf("size\n");
                for(i=0;i<d->dim;i++)
                {
                    fprintf(fphost,"*%s.range[%d]",d->dname,i);
                }
                fprintf(fphost,";\n");
                fprintf(fphost,"\tif(%s_h == NULL){\n",d->dname);
                fprintf(fphost,"\t\t%s_h = (%s *)malloc(%s.size);\n\t}\n",d->dname,d->type,d->dname);
               // fprintf(fphost,"\t%s.ts = 1;\n",d->dname);
                //fprintf(fphost,"\t\t%s.dp = NULL\n}\n",d->dname);
//                fprintf(fphost,"\tif(%s.hp == NULL){\n",d->dname);
//                fprintf(fphost,"\t\t%s.hp = (void *)%s_h;\n\t}\n",d->dname,d->dname);
            }
        }
	}

	return 0;
}
struct datalist * dealdataOMP(struct datalist *p)
{
	struct datalist *d;
	int i;
	if(p == NULL)
		return p;
	else{
		d = p->next;
		i = p->dc;
		fprintf(fphost,"\tstruct dss %s",p->dname);
		while(d != NULL)
		{
			if(d->dc != i)
			{
				if(d->dc == 0)
				{
					d = d->next;
				}
				break;
			}
			else{
				fprintf(fphost,",%s",d->dname);
				d = d->next;
			}
		}
		fputc(';',fphost);
		fputc('\n',fphost);
	}
	return d;
}

int dealrwOMP(struct strlist *p)
{
	struct datalist *dp;
	struct reflist *re,*re0,*retail;
	struct datarefl *dr,*dr0,*drt;
	char *c,*end,*name,*cmp,*cur;
	char tmp;
	int len,i,j,io;
	size_t count;
	c = p->str;
	end = c + p->len;
	tmp = *end;
	*end = '\0';
	//get vars
//	i = 9;//DAC_fill<??
	i = 7;//DAC_rw<
	len = 0;
	re0 = NULL;
//	printf("getvar\n");
	io = 1;//io: 1,read;2,write;3,read-write;
	while(i < p->len)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in DAC_rw<> data definition, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in DAC_rw<> data definition, can not begin with digits\n");
				exit(0);
			}
			else{
				len++;
			}
		}
		else if((c[i] == '_')||((c[i] > 64)&&(c[i] < 91))||((c[i] > 96)&&(c[i] < 123)))
		{
			len++;
		}
		else if((c[i] == ',')||(c[i] == '|')||(c[i] == '>'))
		{
		    if(len > 0)
            {
                name = (char *)malloc(sizeof(char) * (len+1));
                count = i-len;
                for(j=0;j<len;j++)
                {
                    name[j] = c[count];
                    count++;
                }
                name[j] = '\0';
//                printf("%s\n",name);
                re = NULL;
                if(re0 != NULL)
                {
                    re = re0;
                    while(re != NULL)
                    {
                        if(strcmp(re->ref->dname,name) == 0)
                        {
                            if(io & re->iod)
                            {
                                printf("Duplicated declaration of data \'%s\' in DAC_rw<>!\n",name);
                                exit(0);
                            }
                            re->iod |= io;
                            break;
                        }
                        re = re->next;
                    }
                }
                if(re == NULL)
                {
                    dp = dataL;
                    while(dp != NULL)
                    {
                        if(strcmp(dp->dname,name) == 0)
                        {
                            re = (struct reflist *)malloc(sizeof(struct reflist));
                            re->ref = dp;
                            re->iod = io;
                            re->next = NULL;
                            if(re0 == NULL)
                            {
                                re0 = re;
                                retail = re;
                            }
                            else{
                                retail->next = re;
                                retail = re;
                            }
                            break;
                        }
                        dp = dp->next;
                    }
                    if(dp == NULL)
                    {
                        printf("unrecognized data reference \'%s\' in DAC_rw<>! \n",name);
                        free(name);
                        exit(0);
                    }
                }
                free(name);
                len = 0;
            }
            if(c[i] == '>')
                break;
            if(c[i] == '|')
            {
                if(io == 2)
                {
                    printf("wrong use of \'|\' in DAC_rw<> definition!\n");
                    exit(0);
                }
                io = 2;
            }
		}
		else{
			printf("illegal char \'%c\' detected in DAC_rw<>! \n",c[i]);
			exit(0);
		}
		i++;
	}
	while(c[i] != '{')
		i++;
	i++;
	//check ref
//	printf("check ref\n");
	re = re0;
	dr0 = NULL;
	if(re == NULL)
	{
		printf("No data object assigned in DAC_rw<>!\n");
		exit(0);
	}

	while(re != NULL)
	{
		cmp = c+i;
		drt = dr0;
		count = strlen(re->ref->dname);
		while(cmp < end)
		{
			cmp = strstr(cmp,re->ref->dname);
			if((cmp == NULL)||(cmp > end))
			{
				break;
			}
			else{
				if((chkchar(*(cmp-1)) == 1)&&(chkchar(cmp[count]) == 1))
				{
					dr = (struct datarefl *)malloc(sizeof(struct datarefl));
					dr->dp = re->ref;
					dr->pos = cmp;
					dr->next = NULL;
					if(dr0 == NULL)
					{
						dr0 = dr;
						drt = dr;
					}
					else{
						while(drt->next != NULL)
						{
							if(drt->next->pos < cmp)
							{
								drt = drt->next;
							}
							else
								break;
						}
						if(drt->pos > cmp)
						{
							dr->next = drt;
							dr0 = dr;
						}
						else{
							dr->next = drt->next;
							drt->next = dr;
						}
						drt = dr;
					}
				}
			}
			cmp += count;
		}
		re = re->next;
	}
	//check host data ->not needed, no data movement in omp
//	re = re0;
//	while(re != NULL)
//	{
//	    if(re->iod & 1)//read
//            fprintf(fphost,"\tchkhostdata(&%s);\n",re->ref->dname);
//		re = re->next;
//	}
	//copy and deal ref
//	printf("copy deal ref\n");
	dr = dr0;
	cur = c+i;
	while(cur < end)
	{
		if(dr != NULL)
		{
			if(cur < dr->pos)   //copy normal string
			{
				count = dr->pos - cur;
				if(fwrite(cur,count,1,fphost) != 1)
				{
					printf("failed to write host file\n");
					exit(0);
				}
			}
			cur = dr->pos;
			count = strlen(dr->dp->dname);
			if(cur[count] == '[')
			{
				dp = dr->dp;
				fprintf(fphost,"%s_h[",dp->dname);
				for(i=0;i<dp->dim-2;i++)
					fputc('(',fphost);
				for(i=0;i<dp->dim;i++)
				{
					count++;
					while(cur[count] != ']')
					{
						fputc(cur[count],fphost);
						count++;
					}
					if(i<dp->dim-1)
					{
						if(i>1)
							fputc(')',fphost);
						fprintf(fphost,"*%s.range[%d]+",dp->dname,i+1);
					}
					count++;
				}
				fputc(']',fphost);
			}
			else{
                if(dr->dp->dim == 0)
                    fprintf(fphost,"*%s_h",dr->dp->dname);//should be checked
                else
                    fprintf(fphost,"%s_h",dr->dp->dname);//should be checked
			}
			cur += count;
			dr = dr->next;
		}
		else{
			count = end - cur;
			count--;
			if(fwrite(cur,count,1,fphost) != 1)
			{
				printf("failed to write host file\n");
				exit(0);
			}
			break;
		}
	}
//	re = re0;
//	while(re != NULL)
//	{
//	    if(re->iod & 2)//write
//            fprintf(fphost,"\tchkts(&%s);\n",re->ref->dname);
//		re = re->next;
//	}

	*end = tmp;
	free(re0);
	free(dr0);
	return 0;
}

int dealfreeOMP(void)
{
	struct datalist *dp;
	dp = dataL;
	while(dp != NULL)
	{
		fprintf(fphost,"\n\tfree(%s_h);",dp->dname);
		dp = dp->next;
	}
	return 0;
}
