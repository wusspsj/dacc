#include "parse.h"

extern char dacitem[5][6];
extern struct strlist *strL,*strLT;
extern struct datalist *dataL;
extern struct calclist *calcL;
extern struct shelllist *shellL;
extern struct shapelist *shapeL;
extern int icount,spEn;

int chkchar(char c)
{
	if((c < '0')||((c > '9')&&(c < 'A'))||((c > 'Z')&&(c < 'a'))||(c > 'z'))
	{
		if((c != '.')&&(c != '_'))
			return 1;
	}
	return 0;
}
int chkchardot(char c)
{
	if((c < '0')||((c > '9')&&(c < 'A'))||((c > 'Z')&&(c < 'a'))||(c > 'z'))
	{
		if(c != '_')
			return 1;
	}
	return 0;
}
void signstr(char *cur,size_t len,int type)
{
	struct strlist *p;
	p = (struct strlist *)malloc(sizeof(struct strlist));
	p->str = cur;
	p->len = len;
	p->type = type;
	p->next = NULL;
	if(strL == NULL)
	{
		strL = p;
		strLT = p;
	}
	else{
		strLT->next = p;
		strLT = p;
	}
}
void rmcomment(char *start,size_t *slen)
{
	char *cur,*rest;
	size_t offset,len;
	len = *slen;
	cur = start;
	while(*cur != '\0')
	{
		if(*cur == '/')
		{
			if(*(cur+1) == '*')
			{
				//find the first "*/",deal with cur;
				offset = 2;
				while(cur[offset] != '\0')
				{
					if((cur[offset] == '*')&&(cur[offset+1] == '/'))
					{
						offset += 2;
//						if((cur[offset] == 13)&&(cur[offset+1] == 10))//->windows \r\n
//							offset += 2;
//                        else if(cur[offset] == '\n')
//                            offset++;
						break;
					}
					offset++;
				}
				rest = cur + offset;
				if(rest < start+len)
                {
                    strcpy(cur,rest);
                }
                len -= offset;
				cur--;  //point to the last char of the comment. Another ++ after switch.cant point to '\0'
			}
			else if(*(cur+1) == '/')
			{
				//find '\n',the end of the line;and deal with cur;
				offset = 2;
				while((cur[offset] != '\0')&&(cur[offset] != '\n'))
				{
					offset++;
				}
				rest = cur + offset;
				if(rest < start+len)
                {
                    strcpy(cur,rest);
                }
                len -= offset;
				cur--;   //another ++ after switch
			}
		}
		cur++;
	}

	*slen = len;
}
int getstrtype(char *c, int pos)
{
	int stp,i,j;
	int low,high;
	if((*(c+1) == 'A')&&(*(c+2) == 'C')&&(*(c+3) == '_'))
	{
		if(pos == 0)//in main(),data,shape,rw
		{
			low = 0;
			high = 3;
		}
		else{//out of main(),shell,calc
			low = 3;
			high = 5;
		}
		for(i=low;i<high;i++)
		{
			stp = 1+i;
			j = 0;
			while(dacitem[i][j] != '\0')
			{
				if(dacitem[i][j] != c[j+4])
				{
					stp = -1;
				//	printf("\n");
					break;
				}
			//	printf("%c",c[j+4]);
				j++;
			}
			if(stp > 0)
			{
				//stp -= pos<<1;
				//data and shape should be treated differently
				if(stp == 2)//DAC_shape(
				{
					if((chkchar(*(c-1)) == 1)&&(c[j+4] == '('))
					{
						return stp;
					}
				}
				else if(stp == 3)//DAC_rw<
				{
					if((chkchar(*(c-1)) == 1)&&(c[j+4] == '<'))
					{
						return stp;
					}
				}
				else{//data,shell, calc
					if((chkchar(*(c-1)) == 1)&&(chkchar(c[j+4]) == 1))
					{
						return stp;
					}
				}
			}
		}
	}
	return 0;//stp = 1+i; 1,data;2,shape;3,rw;4,shell,5,calc;
}

int getdata(char *cu, int d)//sign data
{
	int i,j,k,len,count,s;
	struct datalist *p,*q;
	char *str,*c;
	i = 0;
	len = 0;
	c = cu;
	while(1)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in DAC_data definition, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in DAC_data definition, can not begin with digits\n");
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
		else if((c[i] == '[')||(c[i] == ',')||(c[i] == ';'))
		{
			p = (struct datalist *)malloc(sizeof(struct datalist));
			p->dname = (char *)malloc(sizeof(char) * (len+1));
			s = i-len;
			p->def = c+s;//?why c+i in initial version ?
			for(j=0;j<len;j++)
			{
				p->dname[j] = c[s];
				s++;
			}
			p->dname[j] = '\0';
			if(dataL == NULL)
			{
				dataL = p;
			}
			else{
				q = dataL;
				while(1){
					//detect duplication of declaration
					if(strcmp(q->dname,p->dname) == 0)
					{
						printf("double declaration of data %s!\n",p->dname);
						exit(0);
					}
					else{
						if(q->next != NULL)
							q = q->next;
						else{
							q->next = p;
							break;
						}
					}
				}
			}
			p->dc = d;
			p->next = NULL;
			p->type = NULL;
			//record ranges if declared
			count = 0;
			if(c[i] == '['){
				s = i;//sign the start of ranges in declaration
				j = 1;
				while(1)
				{
					i++;
					if(c[i] == '[')
					{
						j++;
					}
					else if(c[i] == ']')
					{
						j--;
						if(j != 0)
						{
							printf("Error in DAC_data definition, wrong pattern with []\n");
							exit(0);
						}
						count++;
					}
					else if((c[i] == ',')||(c[i] == ';'))
					{
						break;
					}
				}
			}
			p->dim = count;
			if(count > 0)//sing ranges in declaration
			{
				//start from s
				p->range = (char **)malloc(sizeof(char *) * count);
				for(j=0;j<count;j++)
				{
					len = 0;
					s++;//from [ to range
					while(c[s+len] != ']')
					{
						len++;
					}
					if(len > 0)
					{
						str = (char *)malloc(sizeof(char)*(len+1));
						for(k=0;k<len;k++)
						{
							str[k] = c[s+k];
						}
						str[len] = '\0';
						p->range[j] = str;
					}
					else{
						p->range[j] = NULL;
					}
					s++;//from last ] to next [;
				}
			}
			else{
				p->range = NULL;
			}
			len = 0;
			if(c[i] == ';')
				break;
		}
		else{
			printf("illegal char detected in DAC_data definition\n");
			exit(0);
		}
		i++;
	}
	return i;
}
int getshape(char *cu)
{
	char *str,*tp,*c;
	int i,j,s,count,len;
	struct datalist *dp;
	struct shapelist *sp,*sq;
	i = 0;
	len = 0;
	count = 0;
	c = cu;
	while(1)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in DAC_shape variable, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in DAC_shape, variable can not begin with digits\n");
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
		else if(c[i] == ',')
		{
			s = i-len;
			str = (char *)malloc(sizeof(char)*(len+1));
			for(j=0;j<len;j++)
			{
				str[j] = c[s+j];
			}
			str[j] = '\0';
			dp = dataL;
			while(dp != NULL)
			{
				if(strcmp(dp->dname,str) == 0)
				{
					count = 1;
					break;
				}
				else{
					dp = dp->next;
				}
			}
			if(count != 1)
			{
				printf("Undeclared data name %s in DAC_shape!\n",str);
				exit(0);
			}
			break;
		}
		else{
			printf("illegal char detected in DAC_shape variable name\n");
			exit(0);
		}
		i++;
	}
	len = 0;
	i++;
	sp = (struct shapelist *)malloc(sizeof(struct shapelist));
	sp->data = dp;
	//get type;
	while((c[i]!=',')&&(c[i]!=')'))
	{
		len++;
		i++;
	}
	s = i-len;
	tp = (char *)malloc(sizeof(char)*(len+1));
	for(j=0;j<len;j++)
	{
		tp[j] = c[s+j];
	}
	tp[j] = '\0';
	sp->type = tp;
	i++;
	//get range dim_s
	j = 0;
	len = 0;
	s = i;
	while(c[s] != ';')
	{
		if(c[s] == ',')
		{
			j++;
		}
		else if(c[s] == ')')
		{
			len++;
		}
		else if(c[s] == '(') //in case the range has () expression
		{
			len--;
		}
		s++;
	}
	if(len > 1)
	{
		printf("extra ')' in DAC_shape\n");
		exit(0);
	}
	sp->dim_s = j+len;//
	s = 0;
	if(sp->dim_s > 0)
		sp->range_s = (char **)malloc(sizeof(char *) * sp->dim_s);
	else
		sp->range_s = NULL;
	for(j=0;j<sp->dim_s;j++)
	{
		s = i;
		count = 0;
		while(c[s] != ',')
		{
			if(c[s] == ')')
			{
				count++;
			}
			else if(c[s] == '(') //in case the range has () expression
			{
				count--;
			}
			else if(c[s] == ' ')
            {
                if(len == 0)
                    i++;
            }
            len++;
			if(count == 1)
				break;
			s++;
		}
		len = s-i;
		if(len > 0)
        {
            str = (char *)malloc(sizeof(char)*(len+1));
            len = 0;
            while(i<s)
            {
                str[len] = c[i];
                len++;
                i++;
            }
            str[len] = '\0';
            sp->range_s[j] = str;
		}
		else{
            sp->range_s[j] = NULL;
		}
		i++;
	}
	sp->next = NULL;
	if(shapeL == NULL)
	{
		shapeL = sp;
	}
	else{
		sq = shapeL;
		while(sq->next != NULL)
		{
			sq = sq->next;
		}
		sq->next = sp;
	}
	return 0;
}
size_t checkmain(char *curr)
{
	size_t offset,start;
	int count,i,strtype,dc,c;
	char *cur,*end;
	offset = 0;
	count = 0;
	dc = 1;
	cur = curr;
	printf("checkmain...");
	cur--;
	while(*cur == ' ')
    {
        cur--;
        if(*cur == '\n')
        {
            cur--;
            if(*cur == '\r')
                cur--;
        }
    }
    if((*cur == 't')&&(*(cur-1) == 'n')&&(*(cur-2) == 'i'))
        cur -= 3;
    signstr(cur,0,7);
    cur = curr;
	while(cur[offset] != '\0')
	{
		if(cur[offset] == '{')
			count++;
		else if(cur[offset] == '}')
		{
			count--;
			if(count == 0)
			{
//				printf("end of main\n");
				break;
			}
		}
		else if(cur[offset] == 'D')
		{
			strtype = getstrtype(cur+offset,0);
		//	printf("strtype %d\n",strtype);
	//		printf("%c%c%c\n",cur[offset+1],cur[offset+2],cur[offset+3]);
			switch(strtype)
			{
			case 1://data
				start = offset;
				offset += 9;
				i = getdata(cur+offset,dc);
				offset += i;
				signstr(cur+start,offset-start+1,1);
				dc++;
				break;
			case 2://shape
				start = offset;
				offset += 10;
				getshape(cur+offset);
				while(cur[offset] != ';')
				{
					offset++;
				}
				signstr(cur+start,offset-start+1,2);
				break;
			case 3://rw
				start = offset;
				c = 0;
				while(cur[offset] != '\0')
				{
					if(cur[offset] == '{')
						c++;
					else if(cur[offset] == '}')
					{
						c--;
						if(c == 0)
							break;
					}
					offset++;
				}
				if((cur[offset] == '\0')&&(c != 0))
				{
					printf("no match of { in DAC_io definition!\n");
					exit(0);
				}
				signstr(cur+start,offset-start+1,3);
				break;
			default:
				break;
			}
		}
		else if(cur[offset] == '=')
		{
			if(cur[offset+1] == '>')
			{
				//deal with cur;
				start = offset-1;
				offset += 2;
				while(cur[start] != '\n')
				{
					start--;
				}
				start++;
				while(cur[offset] != '\n')
				{
					offset++;
				}
				signstr(cur+start,offset-start,0);
		//		comptcount++;
			}
		}
		offset++;
	}
	cur = strLT->str + strLT->len;//last special string
	end = curr + offset;//[end] == }
	i = 0;
	//put free at the out most brace
	while(cur < end)
    {
        if(*cur == '{')
            i--;
        else if(*cur == '}')
            i++;
        cur++;
    }
    cur = strLT->str + strLT->len;
    while(i > 0)
    {
        if(*cur == '{')
            i++;
        else if(*cur == '}')
            i--;
        cur++;
    }
    while(cur < end)
    {
        if(*cur == '\n')
            break;
        cur++;
    }
	signstr(cur,0,6);//deal free
	return offset;
}

void signcalc(char *cu, size_t slen)
{
	struct calclist *p,*q;
	struct datalist *dp,*dtail;
	struct strlist *sp,*sq,*stail;
	struct arglist *ap,*aq;
	char *end,*name,*cmp,*c;
	char t[5] = "type";
	char rg[6] = "range";
	int i,j;
	size_t s,len,count;
	char tmp;
	c = cu;
	tmp = c[slen];
	c[slen] = '\0';
	i = 9;
	len = 0;
	//get calc name
	while(i < slen)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in DAC_calc definition, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in DAC_calc, can not begin with digits\n");
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
			s = i-len;
	//		printf("%d ",len);
			name = (char *)malloc(sizeof(char)*(len+1));
			for(j=0;j<len;j++)
			{
				name[j] = c[s+j];
			}
			name[j] = '\0';
			i++;
			break;
		}
		i++;
	}
	if(calcL != NULL)
	{
		p = calcL;
		while(p != NULL)
		{
			if(strcmp(p->cname,name) == 0)
			{
				printf("conflict definition with DAC_calc name: %s\n",name);
				exit(0);
			}
			q = p;
			p = p->next;
		}
	}
	p = (struct calclist *)malloc(sizeof(struct calclist));
	p->cname = name;
//	printf("%s %s\n",name,p->cname);
	p->next = NULL;
	p->vars = NULL;
	p->args = NULL;
	p->strl = NULL;
	p->ker = NULL;
	p->datan = 0;
	p->argn = 0;
	stail = NULL;
	dtail = NULL;
	if(calcL == NULL)
	{
		calcL = p;
	}
	else{
		q->next = p;
	}
	//sign arguments
	s = i;
	j = i;
	while(1)
	{
		if((c[i] == ',')||(c[i] ==')'))
		{
			len = i-1;
			while(c[len] == ' ')
				len--;
			while((c[len] != ' ')&&(len > j))
				len--;
			if(j < len)
			{
				count = len - j;
				ap = (struct arglist *)malloc(sizeof(struct arglist));
				ap->next = NULL;
				ap->nmortp = (char *)malloc(sizeof(char)*(count+1));
				count = 0;
				while(j < len)
				{
					ap->nmortp[count] = c[j];
					j++;
					count++;
				}
				ap->nmortp[count] = '\0';
				if(p->args == NULL)
				{
					p->args = ap;
					aq = ap;
				}
				else{
					aq->next = ap;
					aq = ap;
				}
				p->argn++;
				j = i+1;
			}
			if(c[i] == ')')
				break;
		}
		i++;
	}
	len = i-s;
	if(len > 0)
	{
		sp = (struct strlist *)malloc(sizeof(struct strlist));
		sp->len = len;
		sp->str = c+s;
		sp->type = 10;
		sp->next = NULL;
		if(stail == NULL)
		{
			p->strl = sp;
			stail = p->strl;
		}
		else{
			stail->next = sp;
			stail = sp;
		}
	}
	//record data parameters
	while(c[i] != '<')
	{
		i++;
		if(i > slen)
		{
			printf("no parameter dedected in calc definition.\n");
			exit(0);
		}
	}
	i++;
	len = 0;
	count = 0;
	while(i < slen)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in data definition, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in data definition, can not begin with digits\n");
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
		else if(c[i] == '[')
		{
			while(1)
			{
				if((c[i] == '[')&&(c[i+1] == ']'))
				{
					i += 2;
					count++;
				}
				else if((c[i] == ',')||(c[i] == '>'))
				{
					i--;
					break;
				}
				else{
					printf("Error in DAC_data definition, wrong pattern with []\n");
					exit(0);
				}
			}
		}
		else if((c[i] == ',')||(c[i] == '>'))
		{
			p->datan++;
			dp = (struct datalist *)malloc(sizeof(struct datalist));
			dp->dname = (char *)malloc(sizeof(char) * (len+1));
			s = i-len-count*2;
			for(j=0;j<len;j++)
			{
				dp->dname[j] = c[s+j];
			}
			dp->dname[j] = '\0';
			dp->dim = count;
			dp->next = NULL;
			if(dtail == NULL)
			{
				p->vars = dp;
				dtail = p->vars;
			}
			else{
				dtail->next = dp;
				dtail = dp;
			}
			if(c[i] == ',')
			{
				len = 0;
				count = 0;
			}
			else{
				break;
			}
		}
		else{
			printf("illegal char detected in DAC_data definition\n");
			exit(0);
		}
		i++;
	}
	while(c[i] != '{')
	{
		i++;
		if(i > slen)
		{
			printf("no content dedected in calc definition.\n");
			exit(0);
		}
	}
	p->start = c+i;
	p->clen = slen-i;
	dp = p->vars;
	end = c+slen;
	j = 0;
	while(dp != NULL)
	{
		j++;//j start from 1
		cmp = p->start;
		name = dp->dname;
		len = strlen(name);
		while(cmp < end)
		{
			cmp = strstr(cmp,name);
			if((cmp == NULL)||(cmp > end))
			{
				break;
			}
			if((chkchar(*(cmp-1))==1)&&(chkchardot(cmp[len])==1))//
			{
				sp = (struct strlist *)malloc(sizeof(struct strlist));
				sp->str = cmp;
				sp->len = j;   //indicates the order of the referenced data in the data list,j start from 1,for(i=1;i<j;)
				sp->next = NULL;
				if(cmp[len] == '.')
				{
					if(*(cmp+len+1) == 't')
					{
						s = len+2;
						for(count=1;count<4;count++)
						{
							if(cmp[s] != t[count])
							{
								printf("can not recognize data ref.type\n");
								exit(0);
							}
							s++;
						}
						sp->type = 12;
					}
					else if(*(cmp+len+1) == 'r')//whether need to check range num?
					{
					    s = len+2;
						for(count=1;count<5;count++)
						{
							if(cmp[s] != rg[count])
							{
								printf("can not recognize data ref.range\n");
								exit(0);
							}
							s++;
						}
						sp->type = 13;
					}
					else{
//						printf("can not recognize data ref .\n");
//						exit(0);
						sp->type = 11;//struct reference to struct attributes
					}
				}
				else{//check left and right
					sp->type = 11;   //dataref
				}
				if(stail == NULL)
				{
					p->strl = sp;
					stail = p->strl;
				}
				else{
					if(cmp > stail->str)
					{
						stail->next = sp;
						stail = sp;
					}
					else{
						if(p->strl->str > cmp)
						{
							sp->next = p->strl;
							p->strl = sp;
						}
						else{
							sq = p->strl;
							while(sq->next != NULL)
							{
								if(sq->next->str < cmp)
								{
									sq = sq->next;
								}
								else{
									break;
								}
							}
							sp->next = sq->next;
							sq->next = sp;
						}
					}
				}
			}
			cmp += len;
		}
		dp = dp->next;
	}
	c[slen] = tmp;
}
void signshell(char *cu, size_t slen)
{
	//shelllist
	//strlist
	//dataref data property
	struct shelllist *p,*q;
	struct varlist *vp,*vq,*vt;
//	struct baprop *bap,*bape,*bapt;
	struct ioprop *iop,*iope,*iopt;
	struct splist *splp,*splq,*splt;
	struct indexlist *ip,*iq,*it;
	struct irangelist *irp,*irq;
	struct rangei *rp,*rq;
	//struct spvarlist *spve,*spv;
	//struct propvar *pa,*pat;
	char *name,*cmp,*end,*st,*c;
	int i,j,k,count;
	size_t s,len;
	char tmp;
	c = cu;
	tmp = c[slen];
	c[slen] = '\0';
	end = c+slen;
//	pid = 0;
    //get shell name
	i = 10;//DAC_shell name()<
	len = 0;
	while(i < slen)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in DAC_shell definition, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in DAC_shell, can not begin with digits\n");
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
			s = i-len;
			name = (char *)malloc(sizeof(char)*(len+1));
			for(j=0;j<len;j++)
			{
				name[j] = c[s+j];
			}
			name[j] = '\0';
			break;
		}
		i++;
	}
	if(shellL != NULL)
	{
		p = shellL;
		while(p != NULL)
		{
			if(strcmp(p->shname,name) == 0)
			{
				printf("conflict definition with DAC_shell name: %s\n",name);
				exit(0);
			}
			q = p;
			p = p->next;
		}
	}
	p = (struct shelllist *)malloc(sizeof(struct shelllist));
	p->shname = name;
	p->dnum = 0;
	p->inum = 0;
	p->spnum = 0;
	p->next = NULL;
	p->vars = NULL;
	p->index = NULL;
	p->sp = NULL;
//	p->atomen = 0;
	if(shellL == NULL)
	{
		shellL = p;
	}
	else{
		q->next = p;
	}
	while(c[i] != '<')
	{
		if(i > slen)
		{
			printf("no parameter dedected in shell definition.\n");
			exit(0);
		}
		i++;
	}
	i++;
	printf("shell name:%s...",name);
	vt = NULL;
	len = 0;
	count = 0;
	while(i < slen)
	{
		if(c[i] < 33)
		{
			if(len > 0)
			{
				printf("Error in shell data definition, illegal space char detected\n");
				exit(0);
			}
		}
		else if((c[i] > 47)&&(c[i] < 58))
		{
			if(len == 0)
			{
				printf("Error in shell data definition, can not begin with digits\n");
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
		else if(c[i] == '[')
		{
			while(1)
			{
				if((c[i] == '[')&&(c[i+1] == ']'))
				{
					i += 2;
					count++;
				}
				else if((c[i] == ',')||(c[i] == '>'))
				{
					i--;
					break;
				}
				else{
					printf("Error in shell data definition, wrong pattern with []\n");
					exit(0);
				}
			}
		}
		else if((c[i] == ',')||(c[i] == '>'))
		{
			vp = (struct varlist *)malloc(sizeof(struct varlist));
			vp->vname = (char *)malloc(sizeof(char) * (len+1));
			s = i-len-count*2;
			for(j=0;j<len;j++)
			{
				vp->vname[j] = c[s+j];
			}
			vp->vname[j] = '\0';
			vp->dim = count;
			vp->vid = p->dnum;//vid start from 0,before p->dnum++
			vp->io = 0;
			vp->sis = 0;
			vp->ri = NULL;
			vp->ddname = NULL;
			vp->next = NULL;
			if(p->vars == NULL)
            {
                p->vars = vp;
            }
            else{
                vq = p->vars;
                while(vq != NULL)
                {
                    if(strcmp(vq->vname,vp->vname) == 0)
                    {
                        printf("conflict data declaration in DAC_shell data list: %s\n",vp->vname);
                        exit(0);
                    }
                    vt = vq;
                    vq = vq->next;
                }
                vt->next = vp;
            }
//			printf("%s\n",vp->vname);
			p->dnum++;//does not check if there are repetitive declaration->checked
			if(c[i] == ',')
			{
				len = 0;
				count = 0;
			}
			else{
				break;
			}
		}
		else{
			printf("illegal char detected in shell data definition\n");
			exit(0);
		}
		i++;
	}
//	printf("get vars end...");
	cmp = c+i;
	iop = NULL;
	count = 0;
	while(cmp < end)
	{
		cmp = strstr(cmp,"<=>"); //the IO operator
		if((cmp == NULL)||(cmp > end))
		{
			break;
		}
		else{
			iope = (struct ioprop *)malloc(sizeof(struct ioprop));
			iope->ine = cmp;
			name = cmp;
			iope->id = count;
			count++;
			while(*name != ';')
			{
				name--;
			}
			iope->ins = name;
			name = cmp+3;
			iope->outs = name;
			while(*name != ';')
			{
				name++;
			}
			iope->oute = name;
			iope->next = NULL;
			iope->vars = NULL;
			iope->indexs = NULL;
			//put in the io propety list
			if(iop == NULL)
			{
				iop = iope;
			}
			else{
				iopt->next = iope;
			}
			iopt = iope;
			cmp = name;//set new cmp
		}
	}
	p->ioexp = iop;
	p->ionum = count;
	//get index variables
	cmp = c+i;
	count = 0;
	while(cmp < end)
    {
        cmp = strstr(cmp,"dac_index");
        if((cmp == NULL)||(cmp > end))
            break;
        else{
            if((chkchar(*(cmp-1))==1)&&(chkchar(*(cmp+9))==1))
            {
                cmp += 9;//len(dac_index)
                k = 0;
                len = 0;
                while(cmp[k] != '\n')
                {
                    if(cmp[k] < 33)
                    {
                        if(len > 0)
                        {
                            printf("Error in shell data definition, illegal space char detected\n");
                            exit(0);
                        }
                    }
                    else if((cmp[k] > 47)&&(cmp[k] < 58))
                    {
                        if(len == 0)
                        {
                            printf("Error in shell data definition, can not begin with digits\n");
                            exit(0);
                        }
                        else{
                            len++;
                        }
                    }
                    else if((cmp[k] == '_')||((cmp[k] > 64)&&(cmp[k] < 91))||((cmp[k] > 96)&&(cmp[k] < 123)))
                    {
                        len++;
                    }
                    else if((cmp[k] == ',')||(cmp[k] == ';'))
                    {
                        ip = (struct indexlist *)malloc(sizeof(struct indexlist));
                        ip->iname = (char *)malloc(sizeof(char) * (len+1));
                        s = k-len;
                        for(j=0;j<len;j++)
                        {
                            ip->iname[j] = cmp[s+j];
                        }
                        ip->iname[j] = '\0';
                        printf("index: %s...",ip->iname);
                        ip->ir = NULL;
                        ip->id = count;//id from 0
                        ip->iol = 0;
                        ip->next = NULL;
                        count++;
                        if(p->index == NULL)
                        {
                            p->index = ip;
                        }
                        else{
                            iq = p->index;
                            while(iq != NULL)
                            {
                                if(strcmp(iq->iname,ip->iname) == 0)
                                {
                                    printf("conflict index_var declaration in DAC_shell: %s\n",ip->iname);
                                    exit(0);
                                }
                                it = iq;
                                iq = iq->next;
                            }
                            it->next = ip;
                        }
                        if(cmp[k] == ',')
                        {
                            len = 0;
                        }
                        else{
                            break;
                        }
                    }
                    else{
                        printf("illegal char detected in shell data definition\n");
                        exit(0);
                    }
                    k++;
                }
                cmp += k;
            }
        }
    }
//    p->inum = count;
//    if(icount < count)
//        icount = count;
	//get sp variables
    cmp = c+i;
//	count = 0;//if commented, index_var and sp_var using unified serial number
	while(cmp < end)
    {
        cmp = strstr(cmp,"dac_sp");
        if((cmp == NULL)||(cmp > end))
            break;
        else{
            if((chkchar(*(cmp-1))==1)&&(chkchar(*(cmp+6))==1))
            {
                cmp += 6;//len(dac_sp)
                k = 0;
                len = 0;
                while(cmp[k] != '\n')
                {
                    if(cmp[k] < 33)
                    {
                        if(len > 0)
                        {
                            printf("Error in shell data definition, illegal space char detected\n");
                            exit(0);
                        }
                    }
                    else if((cmp[k] > 47)&&(cmp[k] < 58))
                    {
                        if(len == 0)
                        {
                            printf("Error in shell data definition, can not begin with digits\n");
                            exit(0);
                        }
                        else{
                            len++;
                        }
                    }
                    else if((cmp[k] == '_')||((cmp[k] > 64)&&(cmp[k] < 91))||((cmp[k] > 96)&&(cmp[k] < 123)))
                    {
                        len++;
                    }
                    else if((cmp[k] == ',')||(cmp[k] == ';'))
                    {
                        splp = (struct splist *)malloc(sizeof(struct splist));
                        splp->spname = (char *)malloc(sizeof(char) * (len+1));
                        s = k-len;
                        for(j=0;j<len;j++)
                        {
                            splp->spname[j] = cmp[s+j];
                        }
                        splp->spname[j] = '\0';
                        printf("sp: %s...",splp->spname);
                        splp->ir = NULL;
                        splp->id = count;//id unified
                        splp->iol = 0;
                        splp->next = NULL;
                        count++;
                        if(p->sp == NULL)
                        {
                            p->sp = splp;
                        }
                        else{
                            splq = p->sp;
                            while(splq != NULL)
                            {
                                if(strcmp(splq->spname,splp->spname) == 0)
                                {
                                    printf("conflict sp_var declaration in DAC_shell: %s\n",splp->spname);
                                    exit(0);
                                }
                                splt = splq;
                                splq = splq->next;
                            }
                            splt->next = splp;
                        }
                        //check conflict to index variable
                        if(p->index != NULL)
                        {
                            ip = p->index;
                            while(ip != NULL)
                            {
                                if(strcmp(ip->iname,splp->spname) == 0)
                                {
                                    printf("conflict declaration to index_var in DAC_shell: %s\n",splp->spname);
                                    exit(0);
                                }
                                ip = ip->next;
                            }
                        }
                        if(cmp[k] == ',')
                        {
                            len = 0;
                        }
                        else{
                            break;
                        }
                    }
                    else{
                        printf("illegal char detected in shell data definition\n");
                        exit(0);
                    }
                    k++;
                }
                cmp += k;
            }
        }
    }
//    p->spnum = count;

	//assign the io prop for each variable
//	printf("start assign prop...");
	vp = p->vars;
	while(vp != NULL)
	{
		cmp = c+i;
		s = 0;
		while(cmp < end)
		{
			cmp = strstr(cmp,vp->vname);
			if((cmp == NULL)||(cmp > end))
			{
				break;
			}
			//printf("%d %d %s\n",cmp,end,vp->vname);
			if((chkchar(*(cmp-1))==1)&&(chkchar(*(cmp+strlen(vp->vname)))==1))
			{
			//	printf("in here\n");
				s++;
				if(s > 1)
				{
					printf("variable %s appears twice in shell %s definition\n",vp->vname,p->shname);
					exit(0);
				}
				//assign io property
//				printf("assign io prop\n");
				iope = iop;
				while(iope != NULL)
				{
//				    printf("here1\n");
					if(cmp > iope->oute)
					{
//					    printf("here2\n");
						iope = iope->next;
					}
					else if(cmp < iope->ins){
						break;
					}
					else{
//                        printf("here3\n");
						if(cmp > iope->outs)
						{
							vp->io = 2;//out aka write
						}
						else{
							vp->io = 1;//in aka read
						}
						vp->ioid = iope->id;
//						printf("here4\n");
						break;
					}
				}
				if(vp->io == 0)
				{
					printf("No IO property assigned\n");
					printf("variable %s, in shell %s definition\n",vp->vname,p->shname);
					exit(0);
				}
				irp = (struct irangelist *)malloc(sizeof(struct irangelist));
				irp->var = vp;
				irp->range = iope->id;//.range not used,just initialized
				irp->next = NULL;
//				printf("here5\n");
				if(iope->vars == NULL)
                {
                    iope->vars = irp;
                }
                else{
                    irq = iope->vars;
                    while(irq->next != NULL)
                        irq = irq->next;
                    irq->next = irp;
                }
//                printf("here6\n");
                len = strlen(vp->vname);
//                printf("%s index:",vp->vname);
                st = cmp + len;
                if(*st != '[')
                {
                    if((vp->dim > 0)||(vp->io == 2))//dim > 0 or is written
                    {
                        vp->sis = 1;//atomic
                    }
                }
                else{
                    for(k=0;k<vp->dim;k++)
                    {
                        if(*st != '[')
                        {
                            printf("Wrong pattern of data index in shell definition\n");
                            exit(0);
                        }
                        st++;
                        if(*st != ']')
                        {
                            rp = (struct rangei *)malloc(sizeof(struct rangei));//the indexed ranges in variable,range-indexed
                            rp->id = k;//range id
//                            printf("%d ",k);
                            rp->next = NULL;
                            rp->idl = NULL;
                            rp->spl = NULL;
                            if(vp->ri == NULL)
                            {
                                vp->ri = rp;
                            }
                            else{
                                rq = vp->ri;
                                while(rq->next != NULL)
                                {
                                    rq = rq->next;
                                }
                                rq->next = rp;
                            }
                            count = 0;
                            while(st[count] != ']')
                            {
                                if((st[count] == '\n')||(st[count] == '\0'))
                                {
                                    printf("wrong pattern of data index %s in shell %s\n",vp->vname,p->shname);
                                    exit(0);
                                }
                                count++;
                            }
                            name = (char *)malloc(sizeof(char)*(count+1));
                            for(j=0;j<count;j++)
                            {
                                name[j] = st[j];
                            }
                            name[j] = '\0';
                            st += count;
                            irp = (struct irangelist *)malloc(sizeof(struct irangelist));//ranges binded to the index or sp var
                            irp->var = vp;
                            irp->range = k;
                            irp->next = NULL;
//                            printf("%s ",name);
                            //check index or sp
                            ip = p->index;
                            while(ip != NULL)
                            {
                                if(strcmp(ip->iname,name) == 0)//in index list
                                {
                                    if(ip->ir == NULL)
                                    {
                                        ip->ir = irp;
                                    }
                                    else{
                                        irq = ip->ir;
                                        while(irq->next != NULL)
                                        {
                                            irq = irq->next;
                                        }
                                        irq->next = irp;
                                    }
                                    rp->idl = ip;
                                    vp->sis |= 2;//index
                                    ip->iol |= 1<<vp->ioid;
                                    break;
                                }
                                ip = ip->next;
                            }
                            splp = p->sp;
                            while(splp != NULL)
                            {
                                if(strcmp(splp->spname,name) == 0)
                                {
                                    if(rp->idl != NULL)
                                    {
                                        printf("conflict declaration of variable %s in shell %s definition\n",name,p->shname);
                                        exit(0);
                                    }
                                    if(splp->ir == NULL)
                                    {
                                        splp->ir = irp;
                                    }
                                    else{
                                        irq = splp->ir;
                                        while(irq->next != NULL)
                                        {
                                            irq = irq->next;
                                        }
                                        irq->next = irp;
                                    }
                                    rp->spl = splp;
                                    vp->sis |= 4;//sp
                                    splp->iol |= 1<<vp->ioid;
                                }
                                splp = splp->next;
                            }
                            if((rp->idl == NULL)&&(rp->spl == NULL))
                            {
                                printf("undeclared index or sp var %s in shell %s data %s\n",name,p->shname,vp->vname);
                                exit(0);
                            }
                        }//if(*st != ']')
                        st++;
                    }//for(k)
                    //printf("\n");

                }//else(*st == '[')
                if(vp->io == 2)//detect reduction for each out data
                {
                    if(*st == '(')
                    {
                        st++;
                        count = 0;
                        while(st[count] != ')')
                        {
                            if((st[count] == '\n')||(st[count] == '\0'))
                            {
                                printf("wrong pattern of dedup brackets of data %s in shell %s\n",vp->vname,p->shname);
                                exit(0);
                            }
                            count++;
                        }
                        name = (char *)malloc(sizeof(char)*(count+1));
                        for(j=0;j<count;j++)
                        {
                            name[j] = st[j];
                        }
                        name[j] = '\0';
                        vp->ddname = name;
                    }
                }
			}//if((chkchar(*(cmp-1))==1)&&(chkchar(*(cmp+sizeof(vp->vname)))==1))
			cmp += strlen(vp->vname);
		}//while(cmp < end)
		vp = vp->next;
	}//while(vp!=NULL)
	//remove unused index_var and sp_var, renumber them
//	printf("renumber dimops\n");
	count = 0;
	if(p->index != NULL)
    {
        ip = p->index;
        iq = NULL;
        while(ip != NULL)
        {
            if(ip->ir != NULL)
            {
                ip->id = count;
                count++;
                iq = ip;
                ip = ip->next;
            }
            else{
                if(iq == NULL)//none left
                    p->index = NULL;
                else{
                    iq->next = ip->next;
                    ip = ip->next;
                }
            }
        }
    }
    p->inum = count;

    if(icount < count)
        icount = count;
    if(p->sp != NULL)
    {
        splp = p->sp;
        splq = NULL;
        while(splp != NULL)
        {
            if(splp->ir != NULL)
            {
                splp->id = count;// unified serial number for index and sp identifiers, start from 0
                count++;
                splq = splp;
                splp = splp->next;
            }
            else{
                if(splq == NULL)//none left
                    p->sp = NULL;
                else{
                    splq->next = splp->next;
                    splp = splp->next;
                }
            }
        }
    }
    p->spnum = count - p->inum;
    if((p->inum == 0)&&(p->spnum > 0))
    {
        spEn = 1;
    }
	c[slen] = tmp;
}
