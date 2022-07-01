#include <stdlib.h>
#include <string.h>
#ifdef _MSC_VER
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif
 
#define NNAME 256

/*
 * Return values:
 *
 *       0 = success					 
 *       
 *      -1 = no such environment variable found
 *      
 *      -2 = environment variable name exceeded NNAME chars
 *
 *      -3 = environment variable exceeds lenght of cenv veriable
 *      
 *   other = environment variable is too long for cenv, the value is the
 *           length of buffer necessary to hold the variable
 */
 
void gtenv_(char *cname, char *cenv, int32_t *nenv, int32_t *istat, int32_t lcname, int32_t lcenv)
{
	char   *env;
	char    name[NNAME];
	int32_t i;

	*nenv = 0;
	memset(cenv, ' ', lcenv);

	for (i = lcname-1; i >= 0; --i) if (cname[i] != ' ') break;
	if (i >= NNAME) {
		*istat = -2;
	}
	else {
		strncpy(name, cname, ++i);
		name[i] = '\0';
		if (env = (char *)getenv((const char *)name)) {
			*nenv = strlen(env);
			if (lcenv < *nenv) {
				*istat = -3;
				*nenv = lcenv;
			}
			else {
				*istat = 0;
			}
			strncpy(cenv, env, lcenv);
			if (*istat == 0) memset(&cenv[*nenv], ' ', lcenv - *nenv);

		}
		else {
			*istat = -1;
		}
	}
}

