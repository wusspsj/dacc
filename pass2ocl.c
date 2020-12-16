#include "parse.h"
#include "func.h"
extern struct strlist *strL,*strLT;
extern struct datalist *dataL;
extern struct calclist *calcL;
extern struct shelllist *shellL;
extern struct shapelist *shapeL;
extern FILE *fphost,*fpdev;

char *bfcompt = "\tDAC_krt = (struct kr *)malloc(sizeof(struct kr));\n\
\tDAC_krt->rd = NULL;\n\
\tDAC_krt->oa = NULL;\n\
\tDAC_krt->idx = NULL;\n";

char *afcompt = "\tdealcomptRT(DAC_krt,DAC_platforms);\n\
\tfree(DAC_krt->rd);\n\
\tfree(DAC_krt->idx);\n\
\tfree(DAC_krt->oa);\n\
\tfree(DAC_krt);\n";

char *dealrefc(struct strlist *sp, struct varlist *vp, struct datalist *vars, struct reflist *re, int idxn, int spn)//data reference in calc
{
    char *cur;
    struct splist *spl;
    struct rangei *rip;
    struct indexlist *ip;
    int i,j,k,*ids;

 //   printf("deal refc\n");
    cur = sp->str + strlen(vars->dname);
    fprintf(fpdev,"%s",vars->dname);

    if((vars->dim > 0)&&(*cur != '['))//dim==0 || only name reference
        return cur;
    j = 0;
    if((vp->sis == 1)&&(vp->ddname != NULL))//atom-dup
    {
        ids = (int *)malloc(sizeof(int)*3);
        if(re->idx & 1)
        {
            ids[j] = 0;
            j++;
        }
        if(re->idx & 2)
        {
            ids[j] = 1;
            j++;
        }
        if(re->idx & 4)
        {
            ids[j] = 2;
            j++;
        }
    }
    if((j == 0)&&(vp->dim == 0))//
    {
        //fprintf(fpdev,"*%s",vars->dname);
        return cur;
    }
    //if not, need to complete coordinate transformation
    fputc('[',fpdev);

    k = vp->dim + j;
    for(i=2;i<k;i++)      //should be dim-2
        fputc('(',fpdev);
    if(j > 0)
    {
        switch(j)
        {
        case 1:
            fprintf(fpdev,"tID%d",ids[0]);
            break;
        case 2:
            fprintf(fpdev,"tID%d*%s_r1+tID%d",ids[0],vars->dname,ids[1]);
            break;
        case 3:
            fprintf(fpdev,"tID%d*%s_r1+tID%d)*%s_r2+tID%d",ids[0],vars->dname,ids[1],vars->dname,ids[2]);
            break;
        default:
            break;
        }
        if(vp->dim > 0)
        {
            if(j > 1)
                fputc(')',fpdev);
            fprintf(fpdev,"*%s_r%d",vars->dname,j);
        }
    }

    rip = vp->ri;
    for(i=j;i<k;i++)//ranges one by one
    {
 //       printf("range %d:",i);
        if(i>0)
            fputc('+',fpdev);
        if((rip != NULL)&&(i == rip->id))//indexed range
        {
 //           printf("indexed\n");
            if(rip->idl != NULL)//index
            {
                ip = rip->idl;
                if(ip->id < idxn)//by tid
                {
                    if(i < k-1)
                    {
                        if(i == 0)
                            fprintf(fpdev,"tID%d*%s_r%d",ip->id,vars->dname,i+1);
                        else
                            fprintf(fpdev,"tID%d)*%s_r%d",ip->id,vars->dname,i+1);
                    }
                    else{
                        fprintf(fpdev,"tID%d",ip->id);
                    }
                }
                else{ //by outer ctrl var
                    if(i < k-1)
                    {
                        if(i == 0)
                            fprintf(fpdev,"i_%s*%s_r%d",ip->iname,vars->dname,i+1);  //how to correct this??
                        else
                            fprintf(fpdev,"i_%s)*%s_r%d",ip->iname,vars->dname,i+1);
                    }
                    else{
                        fprintf(fpdev,"i_%s",ip->iname);
                    }
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
                fputc('(',fpdev);
                while(*cur != ']')
                {
                    fputc(*cur,fpdev);
                    cur++;
                }
                if(spl->id < idxn+spn)
                {
                    fprintf(fpdev,"+ %s_r%d/n_%s*tID%d",vars->dname,i,spl->spname,spl->id);// + offset
                }
                fputc(')',fpdev);
                if(i < k-1)
                {
                    if(i == 0)
                        fprintf(fpdev,"*%s_r1",vars->dname);
                    else
                        fprintf(fpdev,")*%s_r%d",vars->dname,i+1);
                }
                cur++;
            }
            rip = rip->next;
        }//if((rip != NULL)&&(i == rip->id))//indexed range
        else{//not indexed
 //           printf("not indexed\n");
            if(*cur != '[')
            {
                printf("unrecognized pattern %c in calc data ref\n",*cur);
                exit(0);
            }
            cur++;
            fputc('(',fpdev);
            while(*cur != ']')
            {
                fputc(*cur,fpdev);
                cur++;
            }
            fputc(')',fpdev);
            if(i < k-1)
            {
                if(i == 0)
                    fprintf(fpdev,"*%s_r1",vars->dname);
                else
                    fprintf(fpdev,")*%s_r%d",vars->dname,i+1);
            }
            cur++;
        }
    }
    fputc(']',fpdev);
//    printf("dealrefc end\n");
    return cur;
}
//return the serial num of a kernel ??how to tell it's a old one or a new one ? unless we create a new one before we return the num
int chkoclkernel(struct reflist *re0, struct calclist *cal, struct shelllist *sh)  // dd: return a kernel name,check kernel should be here,because kernel structure is built in compilation time.
{
	struct reflist *re,*req;
	struct kernellist *k,*kp;
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
	int i,j,count,idxn,spn,ac;//flag not needed, idxn: the number of chosen index, spn: chosen sp
	char *cur,*end,*s,*stsc,*edsc;
	char tmp;
	size_t size;
	count = 1;  //kernel count start from 1
	k = cal->ker;
	kp = cal->ker;
//	printf("chkk\n");
	while(k != NULL)
	{
		if(strcmp(k->sname,sh->shname) == 0)
		{
			i = 0;
			tp = k->t;
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
		kp = k;//kp point to the tail of kernel list
		k = k->next;
		count++;
	}
//	printf("chkk1\n");
	//new kernel
	k = (struct kernellist *)malloc(sizeof(struct kernellist));
	k->next = NULL;
	k->t = NULL;
	k->count = count;
	k->sname = sh->shname;
	re = re0;
	while(re != NULL)
	{
		tp = (struct typelist *)malloc(sizeof(struct typelist));
		tp->next = NULL;
		tp->type = re->ref->type;
		if(k->t == NULL)
		{
			k->t = tp;
			tq = tp;
		}
		else{
			tq->next = tp;
			tq = tp;
		}
		re = re->next;
	}
	if(kp == NULL)
		cal->ker = k;
	else
		kp->next = k;
//	printf("chkk2\n");
	//new kernel here
	fprintf(fpdev,"\n__kernel void %s%d(",cal->cname,count);//kernel name
	//kernel args,data objects first,then deal calc strings one by one

	//normal args first
	sp = cal->strl;
	if(sp->type == 10)
	{
		if(fwrite(sp->str,sp->len,1,fpdev) != 1)
		{
			printf("failed to write dev file\n");
			exit(0);
		}
		fputc(',',fpdev);
		sp = sp->next;
	}
	//data objects
	vp = sh->vars;
	re = re0;
	vars = cal->vars;
	while(re != NULL)  //pass all the dims as args of the kernel
	{
	    j = 0;
		fprintf(fpdev,"__global %s *%s",re->ref->type,vars->dname);
		if((vp->sis == 1)&&(vp->ddname != NULL))
        {
            if(re->idx & 1)
                j++;
            if(re->idx & 2)
                j++;
            if(re->idx & 4)
                j++;
            for(i=0;i<j;i++)
            {
                fprintf(fpdev,",int %s_r%d",vars->dname,i);
            }
        }
		for(i=j;i<vp->dim+j;i++)
		{
			fprintf(fpdev,",int %s_r%d",vars->dname,i);//redundancy exists, can be optimized
		}
//		if(r->iod > 2)
//		{
//			fprintf(fpdev,",int %s_rtid,int %s_d",vars->varname,vars->varname);//%s_d: balance point -> there will be no balance point any more
//		}
		re = re->next;
		vars = vars->next;
		vp = vp->next;
		if(vp != NULL)
			fputc(',',fpdev);
	}
    idxn = 0;
	spn = 0;
	idxn = (sh->inum > 3) ? 3 : sh->inum;
	spn = (idxn+sh->spnum > 3) ? 3 - idxn : sh->spnum;
//    if(spn > sh->spnum)
//        spn = sh->spnum;  //min(3-idxn,sh->spnum)
	//outer control
	if(sh->inum > 3)
	{
		ip = sh->index;
		for(i=0;i<3;i++)
		{
			ip = ip->next;
		}
		while(ip != NULL)
		{
			fprintf(fpdev,",int i_%s",ip->iname); //exceeded index variables, treat as outer control
			ip = ip->next;
		}
	}
	//sign total fraction of every sp identifier
	if(spn > 0)
	{
		spl = sh->sp;
		for(j=0;j<spn;j++)
        {
            fprintf(fpdev,",int n_%s",spl->spname);//number threads of the sp identifier
            spl = spl->next;
        }
	}
	//deal calc string
	fprintf(fpdev,")\n{\n\tsize_t tID0");
	for(j=1;j<idxn+spn;j++)
	{
		fprintf(fpdev,",tID%d",j);
	}

	fprintf(fpdev,";\n");
	for(j=0;j<idxn+spn;j++)
	{
		fprintf(fpdev,"\ttID%d = get_global_id(%d);\n",j,j);
	}
//	if((sh->inum < 3)&&(sh->spnum > 0))
//	{
//		r = re;
//		vars = cal->vars;
//		while(r != NULL)
//		{
//			if(r->iod > 2)
//			{
//				fprintf(fpdev,"%s_pd = (tID%d < %s_d)?1:0;\n",vars->varname,r->tid,vars->varname,r->tid,vars->varname);
//			}
//			r = r->next;
//			vars = vars->next;
//		}
//	}
//	printf("chkk3\n");

    //insert atomic temp variable
    vp = sh->vars;
    re = re0;
    vars = cal->vars;
    j = 1;
    j <<= idxn+spn;
    j--;
    while(vars != NULL)
    {
        if((vp->sis == 1)||((re->idx | re->mask) != j))
        {
            fprintf(fpdev,"\t %s %s_a;\n",re->ref->type,vars->dname);
        }
        vp = vp->next;
        re = re->next;
        vars = vars->next;
    }
//    printf("chkk3\n");
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
                    scluster->len = re->mask;
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
                    scluster->str = stsc;
                    scluster->type = 30;
                    scluster->len = 8;
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
//    printf("chkk4\n");
    //deal data reference
	cur = cal->start+1;
	end = cur + cal->clen;

    scluster = scp;
	while(cur < end)
	{
//	    printf("chkk5\n");
	    while(scluster != NULL)
        {
            if((sp == NULL)||((sp != NULL)&&(scluster->str < sp->str)))
            {
                //printf("here1\n");
                if(cur < scluster->str)
                {
                    size = scluster->str - cur;
                    if(fwrite(cur,size,1,fpdev) != 1)
                    {
                        printf("failed to write dev file\n");
                        exit(0);
                    }
                    cur = scluster->str;
                }
                if(scluster->len == 0)//mask == 0, no mask
                {
                    //do nothing
                   // printf("here2\n");
                    fprintf(fpdev,"\n{\n");
                }
                else if(scluster->len < 8)
                {
                    //printf("here3\n");
                    i = scluster->len;
                    fprintf(fpdev,"\nif");
                    if((i == 1)||(i == 2)||(i == 4))
                        count = 0;
                    else
                        fputc('(',fpdev);
                    j = 1;
                    count = 0;
                    for(i=0;i<3;i++)
                    {
                        if((scluster->len & j) > 0)
                        {
                            if(count > 0)
                                fprintf(fpdev,"&&");
                            fprintf(fpdev,"(tID%d == 0)",i);
                            count++;
                        }
                        j <<= 1;
                    }
                    if(count > 1)
                        fputc(')',fpdev);
                    fprintf(fpdev,"{\n");
                }
                else{
                    //printf("here4\n");
                    fprintf(fpdev,"\n}\n");
                }
				scluster = scluster->next;
            }
            else
                break;
        }
//        printf("chkk6\n");
		if(sp != NULL)
		{
		//	printf("str=%d\n",sp->str);
			if(cur < sp->str)
			{
				size = sp->str - cur;
				if(fwrite(cur,size,1,fpdev) != 1)
				{
					printf("failed to write dev file\n");
					exit(0);
				}
				/*printf("write\n");*/
				cur = sp->str;
			}
			vp = sh->vars;
			vars = cal->vars;
			re = re0;
			for(i=1;i<sp->len;i++)      //the serial num start from 0 or 1?CHECK IT! => start from 1. vp initialization?
			{
				vp = vp->next;
				vars = vars->next;
				re = re->next;
			}
			switch(sp->type)
			{
			case 11:
//				printf("type11 %s\n",vars->dname);
                //deal atomic
                count = 1;
                count <<= idxn+spn;
                count--;
                if(((vp->sis == 1)&&(vp->ddname == NULL))||((re->mask | re->idx) != count))//atomic or explicit atomic, how about atomic-dup????
                {
//                    printf("deal atomic\n");
                    edsc = sp->str;
                    while(edsc < end)
                    {
                        if((*edsc == ';')||(*edsc == '{'))
                            break;
                        else if((*edsc == '\n')&&(*(edsc-1) != '\\'))
                            break;
                        else
                            edsc++;
                    }
                    stsc = sp->str;
                    while(stsc > cal->start)
                    {
                        if((*stsc == ';')||(*stsc == '{')||(*stsc == '}'))
                            break;
                        else if((*stsc == '\n')&&(*(stsc-1) != '\\'))
                            break;
                        else
                            stsc--;
                    }
                    s = stsc;
                    while(s < edsc)
                    {
                        if((*s == '=')&&(*(s+1) != '=')) //find = identifier
                            break;
                        s++;
                    }
                    if((s == edsc)||(s < sp->str))//no =, or in the right
                    {
                        count = 0;
                        cur = sp->str;
                        if((*(cur-1) == '-')&&(*(cur-2) == '-'))
                            count = 1;
                        if((*(cur-1) == '+')&&(*(cur-2) == '+'))
                            count = 3;
                        if(count > 0)
                        {
                            fprintf(fpdev,"\b\b");
                        }
                        cur = sp->str + strlen(vars->dname);
                        for(i=0;i<vars->dim;i++)
                        {
                            while(*cur != ']')
                                cur++;
                            cur++;
                        }
                        if((*cur == '-')&&(*(cur+1) == '-'))
                            count = 2;
                        if((*cur == '+')&&(*(cur+1) == '+'))
                            count = 4;
                        if(count == 0)// no -- or ++
                        {
                            fprintf(fpdev, "atomic_or(");
                        }
                        else{
                            if(count < 3)
                                fprintf(fpdev,"atomic_dec(");
                            else
                                fprintf(fpdev,"atomic_inc(");
                        }
                        if(vp->dim > 0)
                        {
                            fputc('&',fpdev);
                            cur = dealrefc(sp,vp,vars,re,idxn,spn);
                        }
                        else
                            fprintf(fpdev,"%s",vars->dname);
                        if(count == 0)// no -- or ++
                            fprintf(fpdev, ",0)");
                        else{
                            fputc(')',fpdev);
                            if(s < sp->str)
                            {
                                if(count == 1)
                                    fprintf(fpdev,"-1");
                                else if(count == 3)
                                    fprintf(fpdev,"+1");
                            }
                        }
                        if(count > 2)//x-- or x++
                            cur += 2;
                    }
                    else{ // on the left of =
                        count = 0;
                        if((sp->next == NULL)||(sp->next->str > edsc))//no other data ref
                        {
                            switch(*(s-1))
                            {
                            case '+':
                                fprintf(fpdev,"atomic_add(");
                                break;
                            case '-':
                                fprintf(fpdev,"atomic_sub(");
                                break;
                            case '&':
                                fprintf(fpdev,"atomic_and(");
                                break;
                            case '|':
                                fprintf(fpdev,"atomic_or(");
                                break;
                            case '^':
                                fprintf(fpdev,"atomic_xor(");
                                break;
                            default:
                                count = 1;
                                break;
                            }
                            if(count == 1)
                            {
                                fprintf(fpdev,"%s_a",vars->dname);
                                cur = sp->str + strlen(vars->dname);
                            }
                            else{
                                if(vp->dim > 0)
                                {
                                    fputc('&',fpdev);
                                    dealrefc(sp,vp,vars,re,idxn,spn);
                                }
                                else
                                    fprintf(fpdev,"%s",vars->dname);
                                cur = s+1;
                            }

                            while(cur < edsc)
                            {
                                fputc(*cur,fpdev);
                                cur++;
                            }
                            if(count == 0)
                                fputc(')',fpdev);
                            else{
                                fprintf(fpdev,";\natomic_xchg(");
                                if(vp->dim > 0)
                                {
                                    fputc('&',fpdev);
                                    dealrefc(sp,vp,vars,re,idxn,spn);
                                }
                                else
                                    fprintf(fpdev,"%s",vars->dname);
                                fprintf(fpdev,",%s_a)",vars->dname);
                            }
                        }
                        else{// other data ref in the same line

                            sq = sp->next;
                            //first check the min and max expression
                            count = 1;
                            if((sq->len != sp->len)||((sq->next != NULL)&&(sq->next->str < edsc)))
                            {
                                count = 0;
                            }
                            else{
                                tmp = *edsc;
                                *edsc = '\0';
                                stsc = s;
                                s = strstr(stsc,"min");
                                if((s != NULL)&&(s < sq->str))
                                {
                                    count++;
                                }
                                cur = strstr(stsc,"max");
                                if((cur != NULL)&&(cur < sq->str))
                                {
                                    count += 2;
                                }
                                *edsc = tmp;
                                switch(count)
                                {
                                case 1://no max or min
                                    count = 0;
                                    break;
                                case 2://min
                                    count = 1;
                                    break;
                                case 3://max
                                    s = cur;
                                    count = 2;
                                    break;
                                case 4://both,choose the front one
                                    if(s > cur)
                                    {
                                        s = cur;
                                        count = 2;
                                    }
                                    else
                                        count = 1;
                                    break;
                                default:
                                    break;
                                }
                                if(count > 0)// make sure only min( or max( on the right
                                {
                                    cur = stsc;
                                    while(cur < s)
                                    {
                                        if(chkchar(*cur) == 0)
                                        {
                                            count = 0;
                                            break;
                                        }
                                        cur++;
                                    }
                                    if(*(s+3) != '(')
                                        count = 0;
                                }
                                if(count > 0)
                                {
                                    cur = s + 3;
                                    while(cur < edsc)
                                    {
                                        if(*cur == ')')
                                            break;
                                        cur++;
                                    }
                                    if(cur == edsc)
                                        count = 0;
                                    if(cur < sq->str)
                                    {
                                        count = 0;
                                    }
                                    while(cur < edsc)
                                    {
                                        if(chkchar(*cur) == 0)
                                        {
                                            count = 0;
                                            break;
                                        }
                                        cur++;
                                    }
                                    cur = s+4;
                                    i = 0;
                                    while(cur < sq->str)
                                    {
                                        if(*cur == '(')
                                        {
                                            i++;
                                        }
                                        else if(*cur == ')')
                                        {
                                            i--;
                                        }
                                        cur++;
                                    }
                                    if(i != 0)
                                    {
                                        count = 0;
                                    }
                                }
                            }
                            if(count > 0)
                            {
                                cur = s+4;
                                i = 0;
                                while(cur < edsc)
                                {
                                    if(*cur == '(')
                                        i++;
                                    else if(*cur == ')')
                                        i--;
                                    else if(*cur == ',')
                                    {
                                        if(i == 0)
                                            break;
                                    }
                                    cur++;
                                }
                                if(cur == edsc)
                                {
                                    printf("Wrong pattern in min/max expression in DAC_calc %s!!!\n",cal->cname);
                                    exit(0);
                                }
                                stsc = cur;
                                if(cur > sq->str)
                                {
                                    cur = s+4;
                                }
                                while(cur < sq->str)
                                {
                                    if(chkchar(*cur) == 0)
                                    {
                                        count = 0;
                                        break;
                                    }
                                    cur++;
                                }
                                varsq = cal->vars;
                                for(i=1;i<sq->len;i++)
                                {
                                    varsq = varsq->next;
                                }
                                if(varsq->dim > 0)
                                {
                                    for(i=0;i<varsq->dim;i++)
                                    {
                                        while(*cur != ']')
                                            cur++;
                                        cur++;
                                    }
                                }
                                if(stsc > sq->str)
                                {
                                    while(cur < stsc)
                                    {
                                        if(chkchar(*cur) == 0)
                                        {
                                            count = 0;
                                            break;
                                        }
                                        cur++;
                                    }
                                }
                                else{
                                    while(cur < edsc)
                                    {
                                        if(chkchar(*cur) == 0)
                                        {
                                            count = 0;
                                            break;
                                        }
                                        cur++;
                                    }
                                }
                            }
                            if(count > 0)//finally
                            {
                                //
                                if(count == 1)
                                    fprintf(fpdev,"atomic_min(&");
                                else if(count == 2)
                                    fprintf(fpdev,"atomic_max(&");
                                dealrefc(sp,vp,vars,re,idxn,spn);
                                if(stsc > sq->str)
                                {
                                    cur = stsc;
                                    while(cur < edsc)
                                    {
                                        fputc(*cur,fpdev);
                                        cur++;
                                    }
                                }
                                else{
                                    cur = s+4;
                                    fputc(',',fpdev);
                                    while(cur < stsc)
                                    {
                                        fputc(*cur,fpdev);
                                        cur++;
                                    }
                                    fputc(')',fpdev);
                                    cur = edsc;
                                }
                            }
                            else{
                                cur = sp->str + strlen(vars->dname);
                                if(vars->dim > 0)
                                {
                                    for(i=0;i<vars->dim;i++)
                                    {
                                        while(*cur != ']')
                                            cur++;
                                        cur++;
                                    }
                                }
                                fprintf(fpdev,"%s_a",vars->dname);
                                while((*cur != '=')&&(cur < edsc))
                                    cur++;
                                switch(*(cur-1))
                                {
                                case '+':
                                    ac = 1;
                                    break;
                                case '-':
                                    ac = 2;
                                    break;
                                case '&':
                                    ac = 3;
                                    break;
                                case '|':
                                    ac = 4;
                                    break;
                                case '^':
                                    ac = 5;
                                    break;
                                default:
                                    ac = 0;
                                    break;
                                }
                                while((sq != NULL)&&(sq->str < edsc))
                                {
                                    if(cur < sq->str)
                                    {
                                        size = sq->str - cur;
                                        if(fwrite(cur,size,1,fpdev) != 1)
                                        {
                                            printf("failed to write dev file\n");
                                            exit(0);
                                        }
                                        /*printf("write\n");*/
                                    }
                                    vq = sh->vars;
                                    varsq = cal->vars;
                                    req = re0;
                                    for(i=1;i<sq->len;i++)
                                    {
                                        vq = vq->next;
                                        varsq = varsq->next;
                                        req = req->next;
                                    }
                                    count = 1;
                                    count <<= idxn+spn;
                                    count--;
                                    if((vq->sis == 1)||((req->mask | req->idx) != count))
                                    {
                                        fprintf(fpdev,"atomic_or(");
                                        cur = dealrefc(sq,vq,varsq,req,idxn,spn);
                                        fputc(')',fpdev);
                                    }
                                    else
                                        cur = dealrefc(sq,vq,varsq,req,idxn,spn);
                                    sq = sq->next;
                                }
                                while(cur < edsc)
                                {
                                    fputc(*cur,fpdev);
                                    cur++;
                                }
                                fprintf(fpdev,";\n\t");
                                switch(ac)
                                {
                                case 1:
                                    fprintf(fpdev,"atomic_add(");
                                    break;
                                case 2:
                                    fprintf(fpdev,"atomic_sub(");
                                    break;
                                case 3:
                                    fprintf(fpdev,"atomic_and(");
                                    break;
                                case 4:
                                    fprintf(fpdev,"atomic_or(");
                                    break;
                                case 5:
                                    fprintf(fpdev,"atomic_xor(");
                                    break;
                                default:
                                    fprintf(fpdev,"atomic_xchg(");
                                    break;
                                }

                                if(vp->dim > 0)
                                {
                                    fputc('&',fpdev);
                                    dealrefc(sp,vp,vars,re,idxn,spn);
                                }
                                else
                                    fprintf(fpdev,"%s",vars->dname);
                                fprintf(fpdev,",%s_a)",vars->dname);
                            }
                        }
                    }

                    //deal atomic
                    //catch code line
                    //atomic ops: single opd: inc,dec; ->++,--;
                    //            double opd: add,sub,and,or,xor; --> +=,-=,&=,|=,^=;
                    //                        xchg,min,max, --> =, = min, = max;
                    //            triple opd: cmpxchg; --> if(==) =;

                }
                else{
//                    printf("non atomic\n");
                    if(vp->dim > 0)
                    {
                        cur = dealrefc(sp,vp,vars,re,idxn,spn);
                    }
                    else{
                        fprintf(fpdev,"*%s",vars->dname);
                        cur = sp->str + strlen(vars->dname);
                    }
                }
				break;
			case 12: //.type
//				printf("type12\n");
				/*for(i=0;i<10;i++)
					printf("%c",cur[i]);
				printf("\n");*/

				fprintf(fpdev,"%s",re->ref->type);
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
                    printf("Wrong pattern of .range in DAC_calc %s!! char:%c\n",cal->cname,*cur);
                    exit(0);
                }
                cur++;
                while(*cur != ']')
                {
                    if((*cur >= '0')&&(*cur <= '9'))
                    {
                        count *= 10;
                        count += *cur - '0';
                    }
                    else{
                        printf("Wrong pattern of .range in DAC_calc %s!! char:%c\n",cal->cname,*cur);
                        exit(0);
                    }
                    cur++;
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
                                    fprintf(fpdev,"(tID%d == n_%s-1)?",rip->spl->id+idxn,rip->spl->spname);
                                    fprintf(fpdev,"(%s_r%d-%s_r%d/n_%s*tID%d):",vars->dname,i,vars->dname,i,rip->spl->spname,rip->spl->id+idxn);
                                    fprintf(fpdev,"(%s_r%d/n_%s)",vars->dname,i,rip->spl->spname);
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
                                fprintf(fpdev,"%s_r%d",vars->dname,i);
                                break;
                            }
							else
								count--;
						}
					}
					//printf("range: %d\n",i);
				}
				else{
					fprintf(fpdev,"%s_r%d",vars->dname,count);
				}
				break;
			default:
				printf("Wrong str type in calc\n");
				break;
			}//switch(sp->type)
//			printf("chkk7\n");
			while((sp != NULL)&&(cur > sp->str))
            {
                sp = sp->next;
            }
		}//if(sp!=NULL)
		if((sp == NULL)&&(scluster == NULL))//copy the rest
        {
			size = end - cur;
			if(fwrite(cur,size,1,fpdev) != 1)
			{
				printf("failed to write dev file\n");
				exit(0);
			}
			break;
		}
	}
	//fprintf(fphost,"cl_kernel %s%d = createKernel(\"%s%d\");\n",cal->cname,k->count); //CHECK HERE!!!No longer needed!
	return k->count;
}
//check dedup,if not create related kernel create it
int chkocldedup(struct reflist *re,struct varlist *var0,struct shelllist *sh)//return dedup type, 1:reduction;2:merge;0:NULL and Error;
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
	var = ddp->vars;
	fprintf(fpdev,"\n__kernel void %s%d(",var0->ddname,count);
	//sign dedup type
	count = i;

	fprintf(fpdev,"__global %s *%s",tp->type,var->dname);
	atomdup = 0;
	idcount = 0;
	if(var0->sis == 1)//atom-dup
    {
        j = 1;
        ids = (int *)malloc(sizeof(int)*3);
        for(i=0;i<3;i++)
        {
            if(re->idx & j)
            {
                fprintf(fpdev,",int r_%d",idcount);//thread id num
                ids[idcount] = i;
                idcount++;
            }
            j <<= 1;
        }
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
        //
        k = var0->dim;
        ids = (int *)malloc(sizeof(int)*k);
        rgids = (int *)malloc(sizeof(int)*k);
        for(i=0;i<k;i++)
            rgids[i] = -1;
        rgi = var0->ri;
        while(rgi != NULL)
        {
            if(rgi->idl != NULL)
            {
                k = rgi->id;
                rgids[k] = rgi->idl->id;
            }
            else if(rgi->spl != NULL)
            {
                k = rgi->id;
                if(rgi->spl->id < 3)//only the selected sps matter
                {
                    rgids[k] = rgi->spl->id;
                }
            }
            rgi = rgi->next;
        }
        idcount = 0;
        for(i=0;i<var0->dim;i++)
        {
            if(rgids[i] >= 0)
            {
                for(k=0;k<idcount;k++)
                {
                    if(rgids[i] == ids[k])
                        break;
                }
                if(k == idcount)
                {
                    ids[idcount] = rgids[i];
                    idcount++;
                }
            }
        }
    }

	for(i=atomdup;i<re->ref->dim+atomdup;i++)
		fprintf(fpdev,",int r_%d",i);//how about atomic-dup?

    for(i=0;i<idcount;i++)
    {
        if(ids[i] >= sh->inum)
            fprintf(fpdev,",int N_sp%d",i);
    }
    fprintf(fpdev,")\n{\n");
//	fprintf(fpdev,",int r_tID0,int para_d,int thread_N)\n{");//para_d represents balance point,thread_N: total thread num
//	fprintf(fpdev,"size_t tID0 = get_global_id(0);\n");
//	fprintf(fpdev,"size_t sp_hf = sp_d>>1;\n");
//	fprintf(fpdev,"tID0 *= sp_d;\n");
//	fprintf(fpdev,"%s",prededup);
//	fprintf(fpdev,"size_t off_1,off_2,off_3,r_l1,r_l2,r_l3;\nstrobe = 1;\n");

    for(i=0;i<idcount;i++)
    {
        if(i < 3)
        {
            fprintf(fpdev,"\tsize_t tID%d = get_global_id(%d);\n",i,i);
        }
        else{
            fprintf(fpdev,"\tsize_t tID%d = 1;\n",i);
        }
        if((ids[i] < sh->inum)||(atomdup > 0))
            fprintf(fpdev,"\tint STP_%d = 1;\n",i);
        else{
            for(j=0;i<var0->dim;i++)
            {
                if(rgids[j] == ids[i])
                {
                    fprintf(fpdev,"\tint STP_%d = r_%d/N_sp%d;\n",i,j,i);
                    break;
                }
            }
        }
        fprintf(fpdev,"\ttID%d *= STP_%d;\n",i,i);
    }
    if(count == 2)
    {
        fprintf(fpdev,"\tint strobe = 0;\n");
    }
    //generate dedup process
    for(i=idcount-1;i>=0;i--)
    {
        if(i<3)
            fprintf(fpdev,"\ttID%d <<= 1;\n",i);
        fprintf(fpdev,"\twhile(STP_%d < ",i);
        if(atomdup > 0)
        {
            fprintf(fpdev,"r_%d){\n\t\tif(tID%d+STP_%d < r_%d){\n",i,i,i,i);
        }
        else{
            for(j=0;j<var0->dim;j++)
            {
                if(rgids[j] == ids[i])
                    break;
            }
            fprintf(fpdev,"r_%d){\n",j);
            if(i<3)
                fprintf(fpdev,"\t\tif(tID%d+STP_%d < r_%d){\n",i,i,j);
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
 //                   printf("copy...");
                    size = sp->str - cur;
                    if(fwrite(cur,size,1,fpdev) != 1)
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
 //                   printf("11...");
                    fprintf(fpdev,"%s",ddp->vars->dname);
                    if((re->ref->dim > 0)||(atomdup > 0))//maybe no exception??
                    {
                        fputc('[',fpdev);
                    }
                    cur = sp->str + size;
                    for(l=1;l<k;l++)      //should be dim-1,protect 0~n-1
                        fputc('(',fpdev);
                    if(count == 2)//merge
                    {
                        if(sp->len < 3)
                        {
                            fprintf(fpdev,"strobe*r_0+");//strobe or 1-strobe?
                        }
                        else{
                            fprintf(fpdev,"(1-strobe)*r_0+");
                        }
                    }

                    for(l=0;l<k;i++)
                    {
                        if(l > 0)
                        {
                            fprintf(fpdev,"*r_%d+",l);
                        }

                        if(rgids[l] >= 0)//indexed
                        {
                            for(j=0;i<idcount;j++)
                            {
                                if(rgids[l] == ids[j])
                                    break;
                            }
                            if(j > 2)
                            {
                                if(sp->len == 2)
                                    fprintf(fpdev,"STP_%d",j);
                                else
                                    fputc('0',fpdev);
                            }
                            else{
                                fprintf(fpdev,"tID%d",j);
                                if(sp->len == 2)
                                {
                                    fprintf(fpdev,"+STP_%d",j);
                                }
                            }
                            if((atomdup == 0)&&(ids[j] >= sh->inum))
                            {
                                fputc('+',fpdev);
                                if(*cur != '[')
                                {
                                    printf("unrecognized pattern in dedup data ref:%c vs [.\n",*cur);
                                    exit(0);
                                }
                                cur++;
                                while(*cur != ']')
                                {
                                    fputc(*cur,fpdev);
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
                                fputc(*cur,fpdev);
                                cur++;
                            }
                            cur++;
                        }
                        if(l < k-1)
                            fputc(')',fpdev);
                    }
                    fputc(']',fpdev);
                    break;
                case 12:  //type
//                    printf("12...");
                    fprintf(fpdev,"%s",re->ref->type);
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
                        fprintf(fpdev,"r_%d",l);
                    }
                    else{
                        for(j=0;j<idcount;j++)
                        {
                            if(ids[j] == rgids[l])
                                break;
                        }
                        if(sp->len == 2)
                        {
                            fprintf(fpdev,"(tID%d+STP_%d+STP_%d < r_%d ? STP_%d : r_%d-tID%d-STP_%d)",j,j,j,l,j,l,j,j);
                        }
                        else{
                            fprintf(fpdev,"STP_%d",j);
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
                if(fwrite(cur,size,1,fpdev) != 1)
                {
                    printf("failed to write dev file\n");
                    exit(0);
                }
                break;
            }
        }//while(cur < end)
        //
        if(i < 3)
        {
            fprintf(fpdev,"\t\t}\n\t\tSTP_%d <<= 1;\n",i);
            fprintf(fpdev,"\t\ttID%d <<= 1;\n",i);
        }
        else{
            fprintf(fpdev,"\t\tSTP_%d += tID%d;\n",i,i);
        }
        if(count == 2)
        {
            fprintf(fpdev,"\t\tstrobe = 1 - strobe;\n");
        }
        if(i<3)
            fprintf(fpdev,"\t\tbarrier(CLK_GLOBAL_MEM_FENCE);\n\t}\n");
        else
            fprintf(fpdev,"\t}\n\tbarrier(CLK_GLOBAL_MEM_FENCE);\n");
        //fprintf(fpdev,"\t}\n");//end of generated while, synchronization in every loop
//        k++;//???
    }
	fprintf(fpdev,"}\n");
	return count;
}

void dealcomptOCL(struct strlist *p)
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
	struct irangelist *irl;
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

	//check chosen
	//indexsp-chosen-indexed-signed->dedup
	//indexsp-chosen-unindexed->expAtomic
	//atomic-signed->dup
	//check expAtomic

	j =  sh->inum + sh->spnum;
	if(j > 3)
        j = 3;
//    len = 1;
//    len <<= j;
//    len--;//up bound

    re = re0;
	var0 = sh->vars;
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
                re->mask |= 1<<len;//mask : chosen identifiers that not presented
            len++;
        }
		//count idx
		ddflag = 0;
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
                    switch(ri->id)
                    {
                    case 0:
                        re->idx |= 1;
                        break;
                    case 1:
                        re->idx |= 2;
                        break;
                    case 2:
                        re->idx |= 4;
                        break;
                    default:
                        break;
                    }
                }
                ri = ri->next;
            }
        }
        if((var0->sis > 0)&&(var0->ddname != NULL))
        {
            re->ddi = (struct dd *)malloc(sizeof(struct dd));
            re->ddi->ddname = var0->ddname;
            re->ddi->ddflag = chkocldedup(re,var0,sh);
            if(re->ddi->ddflag == 0)
            {
                printf("chkocldedup failed!\n");
                exit(0);
            }
        }
		re = re->next;
		var0 = var0->next;
	}

	//for(i=j;i<3;i++)
	//{
	//	fprintf(fphost,"DAC_krt->a[%d] = 0;\n",i);
	//}

	printf("chkoclkernel...");
	kc = chkoclkernel(re0,cal,sh);
//	printf("chkoclkernel end\n");

	//set kernel runtime data structure,kr,kroa,kril,krda
	fprintf(fphost,"%s\tDAC_krt->ndim = %d;\n",bfcompt,j);
	fprintf(fphost,"\tDAC_krt->inum = %d;\n",sh->inum);
	//fprintf(fphost,"DAC_krt = (struct kr *)malloc(sizeof(struct kr));\n");
	fprintf(fphost,"\tDAC_krt->kname = \"%s%d\";\n",cal->cname,kc);//deal kernel name
	re = re0;
	var0 = sh->vars;
	printf("deal rd...");
	while(re != NULL)// deal run data
	{
		//printf("s,");
		fprintf(fphost,"\tDAC_kda0 = (struct krda *)malloc(sizeof(struct krda));\n\tDAC_kda0->next = NULL;\n");
		fprintf(fphost,"\tDAC_kda0->ds = &%s;\n",re->ref->dname);
		fprintf(fphost,"\tDAC_kda0->iod = %d;\n",re->iod);
		fprintf(fphost,"\tDAC_kda0->sis = %d;\n",var0->sis);
		fprintf(fphost,"\tDAC_kda0->ids = %d;\n",re->idx);
		fprintf(fphost,"\tDAC_kda0->mask = %d;\n",re->mask);

		if(re->ddi != NULL)
		{
			fprintf(fphost,"\tDAC_kda0->ddname = \"%s%d\";\n",re->ddi->ddname,re->ddi->ddc);
			fprintf(fphost,"\tDAC_kda0->ddtype = %d;\n",re->ddi->ddflag);
			fprintf(fphost,"\tDAC_kda0->rgids = (int *)malloc(sizeof(int)*%d);\n",var0->dim);
			for(i=0;i<var0->dim;i++)
            {
                ri = var0->ri;
                if(ri->id == i)
                {
                    if(ri->idl != NULL)
                        fprintf(fphost,"\tDAC_kda0->rgids[%d] = %d;\n",i,ri->idl->id);
                    else
                        fprintf(fphost,"\tDAC_kda0->rgids[%d] = %d;\n",i,ri->spl->id);
                    ri = ri->next;
                }
                else
                    fprintf(fphost,"\tDAC_kda0->rgids[%d] = -1;\n",i);

            }
		}
		else{
			fprintf(fphost,"\tDAC_kda0->ddname = NULL;\n\tDAC_kda0->rgids = NULL;\n\tDAC_kda0->ddtype = 0;\n");
		}
		//printf("5\n");
		fprintf(fphost,"\tif(DAC_krt->rd == NULL)\n\t{\n\t\tDAC_krt->rd = DAC_kda0;\n\t}\n");
		fprintf(fphost,"\telse{\n\t\tDAC_kda1->next = DAC_kda0;\n\t}\n\tDAC_kda1 = DAC_kda0;\n");
		re = re->next;
		var0 = var0->next;
	}
	printf("deal index...");
	//deal index
	//fprintf(fphost,"\tDAC_krt->inum = %d;\n",sh->inum);
	len = (sh->inum >= j) ? sh->inum : j;
	fprintf(fphost,"\tDAC_krt->idx = (struct kril *)malloc(sizeof(struct kril)*%d);\n",len);
	idl = sh->index;
	spl = sh->sp;
	for(i=0;i<len;i++)
    {
        if(idl != NULL)
        {
            re = re0;
			var0 = sh->vars;
			var1 = idl->ir->var;
			while(var0 != var1)
			{
				re = re->next;
				var0 = var0->next;
			}
			fprintf(fphost,"\tDAC_krt->idx[%d].id = %d;\n",i,idl->id);
			fprintf(fphost,"\tDAC_krt->idx[%d].index = %s.range[%d];\n",i,re->ref->dname,idl->ir->range);
            idl = idl->next;
        }
        else{
            fprintf(fphost,"\tDAC_krt->idx[%d].id = %d;\n",i,spl->id);
			fprintf(fphost,"\tDAC_krt->idx[%d].index = ",i);

			irl = spl->ir;
			while(irl->next != NULL)
			{
			    var1 = irl->var;
			    re = re0;
                var0 = sh->vars;
                while(var0 != var1)
                {
                    re = re->next;
                    var0 = var0->next;
                }
                fprintf(fphost,"min(%s.range[%d],",re->ref->dname,irl->range);
                irl = irl->next;
			}
			var1 = irl->var;
            re = re0;
            var0 = sh->vars;
            while(var0 != var1)
            {
                re = re->next;
                var0 = var0->next;
            }
            fprintf(fphost,"%s.range[%d]",re->ref->dname,irl->range);
            irl = spl->ir;
            while(irl->next != NULL)
                fputc(')',fphost);
            fputc(';',fphost);
            fputc('\n',fphost);

            spl = spl->next;
        }
    }

	//deal original args
	printf("deal oa...");
	if(argn != 0)
	{
		ap = ap0;
		aptail = cal->args;
		while(ap != NULL)
		{
			fprintf(fphost,"\tDAC_koa0 = (struct kroa *)malloc(sizeof(struct kroa));\n\tDAC_koa0->next = NULL;\n");
			fprintf(fphost,"\tDAC_koa0->arg = &(%s);\n\tDAC_koa0->size = sizeof(%s);\n",ap->nmortp,aptail->nmortp);
			fprintf(fphost,"\tif(DAC_krt->oa == NULL)\n\t\tDAC_krt->oa = DAC_koa0;\n");
			fprintf(fphost,"\telse\n\t\tDAC_koa1->next = DAC_koa0;\n\tDAC_koa1 = DAC_koa0;\n");
			ap = ap->next;
			aptail = aptail->next;
		}
	}
	fprintf(fphost,"%s",afcompt);
	free(re0);
}

int dealshapeOCL(struct shapelist *p)
{
	struct datalist *d;
	int i,f;
	d = p->data;

	if(d->type == NULL)//first define;
	{
		d->type = p->type;
		fprintf(fphost,"%s *%s_h;\n",d->type,d->dname);
		fprintf(fphost,"\t%s_h = NULL;\n",d->dname);
		fprintf(fphost,"\t%s.hp = NULL;\n",d->dname);
		fprintf(fphost,"\t%s.ts = 0;\n",d->dname);
		fprintf(fphost,"\t%s.dp = NULL;\n",d->dname);
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
            fprintf(fphost,"\tif(%s.hp == NULL){\n",d->dname);
            fprintf(fphost,"\t\t%s.hp = (void *)%s_h;\n\t}\n",d->dname,d->dname);
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
//		if((d->dim == 0)&&(p->dim_s > 0))
//        {
//            d->dim = p->dim_s;
//            fprintf(fphost,"%s.dim = %d;\n",d->dname,d->dim);
//            fprintf(fphost,"%s.range = (size_t *)malloc(sizeof(size_t)*%d);\n",d->dname,d->dim);
//			fprintf(fphost,"%s.range_u = (size_t *)malloc(sizeof(size_t)*%d);\n",d->dname,d->dim);
//			d->range = p->range_s;
//			for(i=0;i<d->dim;i++)
//            {
//                fprintf(fphost,"%s.range_u[%d] = %s;\n",d->dname,i,p->range_s[i]);
//                fprintf(fphost,"%s.range[%d] = %s;\n",d->dname,i,p->range_s[i]);
//            }
//            fprintf(fphost,"%s.size = %s.type",d->dname,d->dname);
//        //	printf("size\n");
//            for(i=0;i<d->dim;i++)
//            {
//                fprintf(fphost,"*%s.range[%d]",d->dname,i);
//            }
//            fprintf(fphost,";\n");
//            fprintf(fphost,"if(%s_h == NULL)\n",d->dname);
//            fprintf(fphost,"\t%s_h = (%s *)malloc(%s.size);\n",d->dname,d->type,d->dname);
//        }
//        else if(d->dim > 0)
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
                fprintf(fphost,"\tif(%s.hp == NULL){\n",d->dname);
                fprintf(fphost,"\t\t%s.hp = (void *)%s_h;\n\t}\n",d->dname,d->dname);
            }
        }
	}

	return 0;
}
struct datalist * dealdataOCL(struct datalist *p)
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

int dealrwOCL(struct strlist *p)
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
	re = re0;
	while(re != NULL)
	{
	    if(re->iod & 1)//read
            fprintf(fphost,"\tchkhostdata(&%s);\n",re->ref->dname);
		re = re->next;
	}
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
	re = re0;
	while(re != NULL)
	{
	    if(re->iod & 2)//write
            fprintf(fphost,"\tchkts(&%s);\n",re->ref->dname);
		re = re->next;
	}

	*end = tmp;
	free(re0);
	free(dr0);
	return 0;
}

int dealfreeOCL(void)
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
