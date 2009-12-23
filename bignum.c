/**********************************************************************

  bignum.c -

  $Author: yugui $
  created at: Fri Jun 10 00:48:55 JST 1994

  Copyright (C) 1993-2007 Yukihiro Matsumoto

**********************************************************************/

#include "ruby/ruby.h"

#include <math.h>
#include <float.h>
#include <ctype.h>
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

VALUE rb_cBignum;

#if defined __MINGW32__
#define USHORT _USHORT
#endif

#define BDIGITS(x) (RBIGNUM_DIGITS(x))
#define BITSPERDIG (SIZEOF_BDIGITS*CHAR_BIT)
#define BIGRAD ((BDIGIT_DBL)1 << BITSPERDIG)
#define DIGSPERLONG ((unsigned int)(SIZEOF_LONG/SIZEOF_BDIGITS))
#if HAVE_LONG_LONG
# define DIGSPERLL ((unsigned int)(SIZEOF_LONG_LONG/SIZEOF_BDIGITS))
#endif
#define BIGUP(x) ((BDIGIT_DBL)(x) << BITSPERDIG)
#define BIGDN(x) RSHIFT(x,BITSPERDIG)
#define BIGLO(x) ((BDIGIT)((x) & (BIGRAD-1)))
#define BDIGMAX ((BDIGIT)-1)

#define BIGZEROP(x) (RBIGNUM_LEN(x) == 0 || \
		     (BDIGITS(x)[0] == 0 && \
		      (RBIGNUM_LEN(x) == 1 || bigzero_p(x))))

static int
bigzero_p(VALUE x)
{
    long i;
    for (i = RBIGNUM_LEN(x) - 1; 0 <= i; i--) {
	if (BDIGITS(x)[i]) return 0;
    }
    return 1;
}

int
rb_cmpint(VALUE val, VALUE a, VALUE b)
{
    if (NIL_P(val)) {
	rb_cmperr(a, b);
    }
    if (FIXNUM_P(val)) {
        long l = FIX2LONG(val);
        if (l > 0) return 1;
        if (l < 0) return -1;
        return 0;
    }
    if (TYPE(val) == T_BIGNUM) {
	if (BIGZEROP(val)) return 0;
	if (RBIGNUM_SIGN(val)) return 1;
	return -1;
    }
    if (RTEST(rb_funcall(val, '>', 1, INT2FIX(0)))) return 1;
    if (RTEST(rb_funcall(val, '<', 1, INT2FIX(0)))) return -1;
    return 0;
}

#define RBIGNUM_SET_LEN(b,l) \
  ((RBASIC(b)->flags & RBIGNUM_EMBED_FLAG) ? \
   (RBASIC(b)->flags = (RBASIC(b)->flags & ~RBIGNUM_EMBED_LEN_MASK) | \
      ((l) << RBIGNUM_EMBED_LEN_SHIFT)) : \
   (RBIGNUM(b)->as.heap.len = (l)))

static void
rb_big_realloc(VALUE big, long len)
{
    BDIGIT *ds;
    if (RBASIC(big)->flags & RBIGNUM_EMBED_FLAG) {
	if (RBIGNUM_EMBED_LEN_MAX < len) {
	    ds = ALLOC_N(BDIGIT, len);
	    MEMCPY(ds, RBIGNUM(big)->as.ary, BDIGIT, RBIGNUM_EMBED_LEN_MAX);
	    RBIGNUM(big)->as.heap.len = RBIGNUM_LEN(big);
	    RBIGNUM(big)->as.heap.digits = ds;
	    RBASIC(big)->flags &= ~RBIGNUM_EMBED_FLAG;
	}
    }
    else {
	if (len <= RBIGNUM_EMBED_LEN_MAX) {
	    ds = RBIGNUM(big)->as.heap.digits;
	    RBASIC(big)->flags |= RBIGNUM_EMBED_FLAG;
	    RBIGNUM_SET_LEN(big, len);
	    if (ds) {
		MEMCPY(RBIGNUM(big)->as.ary, ds, BDIGIT, len);
		xfree(ds);
	    }
	}
	else {
	    if (RBIGNUM_LEN(big) == 0) {
		RBIGNUM(big)->as.heap.digits = ALLOC_N(BDIGIT, len);
	    }
	    else {
		REALLOC_N(RBIGNUM(big)->as.heap.digits, BDIGIT, len);
	    }
	}
    }
}

void
rb_big_resize(VALUE big, long len)
{
    rb_big_realloc(big, len);
    RBIGNUM_SET_LEN(big, len);
}

static VALUE
bignew_1(VALUE klass, long len, int sign)
{
    NEWOBJ(big, struct RBignum);
    OBJSETUP(big, klass, T_BIGNUM);
    RBIGNUM_SET_SIGN(big, sign?1:0);
    if (len <= RBIGNUM_EMBED_LEN_MAX) {
	RBASIC(big)->flags |= RBIGNUM_EMBED_FLAG;
	RBIGNUM_SET_LEN(big, len);
    }
    else {
	rb_big_resize((VALUE)big, len);
    }

    return (VALUE)big;
}

#define bignew(len,sign) bignew_1(rb_cBignum,len,sign)

VALUE
rb_big_clone(VALUE x)
{
    VALUE z = bignew_1(CLASS_OF(x), RBIGNUM_LEN(x), RBIGNUM_SIGN(x));

    MEMCPY(BDIGITS(z), BDIGITS(x), BDIGIT, RBIGNUM_LEN(x));
    return z;
}

/* modify a bignum by 2's complement */
static void
get2comp(VALUE x)
{
    long i = RBIGNUM_LEN(x);
    BDIGIT *ds = BDIGITS(x);
    BDIGIT_DBL num;

    if (!i) return;
    while (i--) ds[i] = ~ds[i];
    i = 0; num = 1;
    do {
	num += ds[i];
	ds[i++] = BIGLO(num);
	num = BIGDN(num);
    } while (i < RBIGNUM_LEN(x));
    if (num != 0) {
	rb_big_resize(x, RBIGNUM_LEN(x)+1);
	ds = BDIGITS(x);
	ds[RBIGNUM_LEN(x)-1] = 1;
    }
}

void
rb_big_2comp(VALUE x)			/* get 2's complement */
{
    get2comp(x);
}

static VALUE
bigtrunc(VALUE x)
{
    long len = RBIGNUM_LEN(x);
    BDIGIT *ds = BDIGITS(x);

    if (len == 0) return x;
    while (--len && !ds[len]);
    rb_big_resize(x, len+1);
    return x;
}

static VALUE
bigfixize(VALUE x)
{
    long len = RBIGNUM_LEN(x);
    BDIGIT *ds = BDIGITS(x);

    if (len*SIZEOF_BDIGITS <= sizeof(long)) {
	long num = 0;
	while (len--) {
	    num = BIGUP(num) + ds[len];
	}
	if (num >= 0) {
	    if (RBIGNUM_SIGN(x)) {
		if (POSFIXABLE(num)) return LONG2FIX(num);
	    }
	    else {
		if (NEGFIXABLE(-(long)num)) return LONG2FIX(-(long)num);
	    }
	}
    }
    return x;
}

static VALUE
bignorm(VALUE x)
{
    if (!FIXNUM_P(x) && TYPE(x) == T_BIGNUM) {
	x = bigfixize(bigtrunc(x));
    }
    return x;
}

VALUE
rb_big_norm(VALUE x)
{
    return bignorm(x);
}

VALUE
rb_uint2big(VALUE n)
{
    BDIGIT_DBL num = n;
    long i = 0;
    BDIGIT *digits;
    VALUE big;

    big = bignew(DIGSPERLONG, 1);
    digits = BDIGITS(big);
    while (i < DIGSPERLONG) {
	digits[i++] = BIGLO(num);
	num = BIGDN(num);
    }

    i = DIGSPERLONG;
    while (--i && !digits[i]) ;
    RBIGNUM_SET_LEN(big, i+1);
    return big;
}

VALUE
rb_int2big(SIGNED_VALUE n)
{
    long neg = 0;
    VALUE big;

    if (n < 0) {
	n = -n;
	neg = 1;
    }
    big = rb_uint2big(n);
    if (neg) {
	RBIGNUM_SET_SIGN(big, 0);
    }
    return big;
}

VALUE
rb_uint2inum(VALUE n)
{
    if (POSFIXABLE(n)) return LONG2FIX(n);
    return rb_uint2big(n);
}

VALUE
rb_int2inum(SIGNED_VALUE n)
{
    if (FIXABLE(n)) return LONG2FIX(n);
    return rb_int2big(n);
}

#ifdef HAVE_LONG_LONG

void
rb_quad_pack(char *buf, VALUE val)
{
    LONG_LONG q;

    val = rb_to_int(val);
    if (FIXNUM_P(val)) {
	q = FIX2LONG(val);
    }
    else {
	long len = RBIGNUM_LEN(val);
	BDIGIT *ds;

	if (len > SIZEOF_LONG_LONG/SIZEOF_BDIGITS) {
	    len = SIZEOF_LONG_LONG/SIZEOF_BDIGITS;
	}
	ds = BDIGITS(val);
	q = 0;
	while (len--) {
	    q = BIGUP(q);
	    q += ds[len];
	}
	if (!RBIGNUM_SIGN(val)) q = -q;
    }
    memcpy(buf, (char*)&q, SIZEOF_LONG_LONG);
}

VALUE
rb_quad_unpack(const char *buf, int sign)
{
    unsigned LONG_LONG q;
    long neg = 0;
    long i;
    BDIGIT *digits;
    VALUE big;

    memcpy(&q, buf, SIZEOF_LONG_LONG);
    if (sign) {
	if (FIXABLE((LONG_LONG)q)) return LONG2FIX((LONG_LONG)q);
	if ((LONG_LONG)q < 0) {
	    q = -(LONG_LONG)q;
	    neg = 1;
	}
    }
    else {
	if (POSFIXABLE(q)) return LONG2FIX(q);
    }

    i = 0;
    big = bignew(DIGSPERLL, 1);
    digits = BDIGITS(big);
    while (i < DIGSPERLL) {
	digits[i++] = BIGLO(q);
	q = BIGDN(q);
    }

    i = DIGSPERLL;
    while (i-- && !digits[i]) ;
    RBIGNUM_SET_LEN(big, i+1);

    if (neg) {
	RBIGNUM_SET_SIGN(big, 0);
    }
    return bignorm(big);
}

#else

#define QUAD_SIZE 8

void
rb_quad_pack(char *buf, VALUE val)
{
    long len;

    memset(buf, 0, QUAD_SIZE);
    val = rb_to_int(val);
    if (FIXNUM_P(val)) {
	val = rb_int2big(FIX2LONG(val));
    }
    len = RBIGNUM_LEN(val) * SIZEOF_BDIGITS;
    if (len > QUAD_SIZE) {
	rb_raise(rb_eRangeError, "bignum too big to convert into `quad int'");
    }
    memcpy(buf, (char*)BDIGITS(val), len);
    if (!RBIGNUM_SIGN(val)) {
	len = QUAD_SIZE;
	while (len--) {
	    *buf = ~*buf;
	    buf++;
	}
    }
}

#define BNEG(b) (RSHIFT(((BDIGIT*)b)[QUAD_SIZE/SIZEOF_BDIGITS-1],BITSPERDIG-1) != 0)

VALUE
rb_quad_unpack(const char *buf, int sign)
{
    VALUE big = bignew(QUAD_SIZE/SIZEOF_BDIGITS, 1);

    memcpy((char*)BDIGITS(big), buf, QUAD_SIZE);
    if (sign && BNEG(buf)) {
	long len = QUAD_SIZE;
	char *tmp = (char*)BDIGITS(big);

	RBIGNUM_SET_SIGN(big, 0);
	while (len--) {
	    *tmp = ~*tmp;
	    tmp++;
	}
    }

    return bignorm(big);
}

#endif

VALUE
rb_cstr_to_inum(const char *str, int base, int badcheck)
{
    const char *s = str;
    char *end;
    char sign = 1, nondigit = 0;
    int c;
    BDIGIT_DBL num;
    long len, blen = 1;
    long i;
    VALUE z;
    BDIGIT *zds;

#define conv_digit(c) \
    (!ISASCII(c) ? -1 : \
     ISDIGIT(c) ? ((c) - '0') : \
     ISLOWER(c) ? ((c) - 'a' + 10) : \
     ISUPPER(c) ? ((c) - 'A' + 10) : \
     -1)

    if (!str) {
	if (badcheck) goto bad;
	return INT2FIX(0);
    }
    while (ISSPACE(*str)) str++;

    if (str[0] == '+') {
	str++;
    }
    else if (str[0] == '-') {
	str++;
	sign = 0;
    }
    if (str[0] == '+' || str[0] == '-') {
	if (badcheck) goto bad;
	return INT2FIX(0);
    }
    if (base <= 0) {
	if (str[0] == '0') {
	    switch (str[1]) {
	      case 'x': case 'X':
		base = 16;
		break;
	      case 'b': case 'B':
		base = 2;
		break;
	      case 'o': case 'O':
		base = 8;
		break;
	      case 'd': case 'D':
		base = 10;
		break;
	      default:
		base = 8;
	    }
	}
	else if (base < -1) {
	    base = -base;
	}
	else {
	    base = 10;
	}
    }
    switch (base) {
      case 2:
	len = 1;
	if (str[0] == '0' && (str[1] == 'b'||str[1] == 'B')) {
	    str += 2;
	}
	break;
      case 3:
	len = 2;
	break;
      case 8:
	if (str[0] == '0' && (str[1] == 'o'||str[1] == 'O')) {
	    str += 2;
	}
      case 4: case 5: case 6: case 7:
	len = 3;
	break;
      case 10:
	if (str[0] == '0' && (str[1] == 'd'||str[1] == 'D')) {
	    str += 2;
	}
      case 9: case 11: case 12: case 13: case 14: case 15:
	len = 4;
	break;
      case 16:
	len = 4;
	if (str[0] == '0' && (str[1] == 'x'||str[1] == 'X')) {
	    str += 2;
	}
	break;
      default:
	if (base < 2 || 36 < base) {
	    rb_raise(rb_eArgError, "invalid radix %d", base);
	}
	if (base <= 32) {
	    len = 5;
	}
	else {
	    len = 6;
	}
	break;
    }
    if (*str == '0') {		/* squeeze preceding 0s */
	int us = 0;
	while ((c = *++str) == '0' || c == '_') {
	    if (c == '_') {
		if (++us >= 2)
		    break;
	    } else
		us = 0;
	}
	if (!(c = *str) || ISSPACE(c)) --str;
    }
    c = *str;
    c = conv_digit(c);
    if (c < 0 || c >= base) {
	if (badcheck) goto bad;
	return INT2FIX(0);
    }
    len *= strlen(str)*sizeof(char);

    if (len <= (sizeof(long)*CHAR_BIT)) {
	unsigned long val = STRTOUL(str, &end, base);

	if (str < end && *end == '_') goto bigparse;
	if (badcheck) {
	    if (end == str) goto bad; /* no number */
	    while (*end && ISSPACE(*end)) end++;
	    if (*end) goto bad;	      /* trailing garbage */
	}

	if (POSFIXABLE(val)) {
	    if (sign) return LONG2FIX(val);
	    else {
		long result = -(long)val;
		return LONG2FIX(result);
	    }
	}
	else {
	    VALUE big = rb_uint2big(val);
	    RBIGNUM_SET_SIGN(big, sign);
	    return bignorm(big);
	}
    }
  bigparse:
    len = (len/BITSPERDIG)+1;
    if (badcheck && *str == '_') goto bad;

    z = bignew(len, sign);
    zds = BDIGITS(z);
    for (i=len;i--;) zds[i]=0;
    while ((c = *str++) != 0) {
	if (c == '_') {
	    if (nondigit) {
		if (badcheck) goto bad;
		break;
	    }
	    nondigit = c;
	    continue;
	}
	else if ((c = conv_digit(c)) < 0) {
	    break;
	}
	if (c >= base) break;
	nondigit = 0;
	i = 0;
	num = c;
	for (;;) {
	    while (i<blen) {
		num += (BDIGIT_DBL)zds[i]*base;
		zds[i++] = BIGLO(num);
		num = BIGDN(num);
	    }
	    if (num) {
		blen++;
		continue;
	    }
	    break;
	}
    }
    if (badcheck) {
	str--;
	if (s+1 < str && str[-1] == '_') goto bad;
	while (*str && ISSPACE(*str)) str++;
	if (*str) {
	  bad:
	    rb_invalid_str(s, "Integer");
	}
    }

    return bignorm(z);
}

VALUE
rb_str_to_inum(VALUE str, int base, int badcheck)
{
    char *s;
    long len;

    StringValue(str);
    if (badcheck) {
	s = StringValueCStr(str);
    }
    else {
	s = RSTRING_PTR(str);
    }
    if (s) {
	len = RSTRING_LEN(str);
	if (s[len]) {		/* no sentinel somehow */
	    char *p = ALLOCA_N(char, len+1);

	    MEMCPY(p, s, char, len);
	    p[len] = '\0';
	    s = p;
	}
    }
    return rb_cstr_to_inum(s, base, badcheck);
}

#if HAVE_LONG_LONG

static VALUE
rb_ull2big(unsigned LONG_LONG n)
{
    BDIGIT_DBL num = n;
    long i = 0;
    BDIGIT *digits;
    VALUE big;

    big = bignew(DIGSPERLL, 1);
    digits = BDIGITS(big);
    while (i < DIGSPERLL) {
	digits[i++] = BIGLO(num);
	num = BIGDN(num);
    }

    i = DIGSPERLL;
    while (i-- && !digits[i]) ;
    RBIGNUM_SET_LEN(big, i+1);
    return big;
}

static VALUE
rb_ll2big(LONG_LONG n)
{
    long neg = 0;
    VALUE big;

    if (n < 0) {
	n = -n;
	neg = 1;
    }
    big = rb_ull2big(n);
    if (neg) {
	RBIGNUM_SET_SIGN(big, 0);
    }
    return big;
}

VALUE
rb_ull2inum(unsigned LONG_LONG n)
{
    if (POSFIXABLE(n)) return LONG2FIX(n);
    return rb_ull2big(n);
}

VALUE
rb_ll2inum(LONG_LONG n)
{
    if (FIXABLE(n)) return LONG2FIX(n);
    return rb_ll2big(n);
}

#endif  /* HAVE_LONG_LONG */

VALUE
rb_cstr2inum(const char *str, int base)
{
    return rb_cstr_to_inum(str, base, base==0);
}

VALUE
rb_str2inum(VALUE str, int base)
{
    return rb_str_to_inum(str, base, base==0);
}

const char ruby_digitmap[] = "0123456789abcdefghijklmnopqrstuvwxyz";

static VALUE bigsqr(VALUE x);
static void bigdivmod(VALUE x, VALUE y, VALUE *divp, VALUE *modp);

#define POW2_P(x) (((x)&((x)-1))==0)

static inline int
ones(register unsigned long x)
{
#if SIZEOF_LONG == 8
# define MASK_55 0x5555555555555555UL
# define MASK_33 0x3333333333333333UL
# define MASK_0f 0x0f0f0f0f0f0f0f0fUL
#else
# define MASK_55 0x55555555UL
# define MASK_33 0x33333333UL
# define MASK_0f 0x0f0f0f0fUL
#endif
    x -= (x >> 1) & MASK_55;
    x = ((x >> 2) & MASK_33) + (x & MASK_33);
    x = ((x >> 4) + x) & MASK_0f;
    x += (x >> 8);
    x += (x >> 16);
#if SIZEOF_LONG == 8
    x += (x >> 32);
#endif
    return (int)(x & 0x7f);
#undef MASK_0f
#undef MASK_33
#undef MASK_55
}

static inline unsigned long
next_pow2(register unsigned long x)
{
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
#if SIZEOF_LONG == 8
    x |= x >> 32;
#endif
    return x + 1;
}

static inline int
floor_log2(register unsigned long x)
{
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
#if SIZEOF_LONG == 8
    x |= x >> 32;
#endif
    return (int)ones(x) - 1;
}

static inline int
ceil_log2(register unsigned long x)
{
    return floor_log2(x) + !POW2_P(x);
}

#define LOG2_KARATSUBA_DIGITS 7
#define KARATSUBA_DIGITS (1L<<LOG2_KARATSUBA_DIGITS)
#define MAX_BIG2STR_TABLE_ENTRIES 64

static VALUE big2str_power_cache[35][MAX_BIG2STR_TABLE_ENTRIES];

static void
power_cache_init(void)
{
    int i, j;
    for (i = 0; i < 35; ++i) {
	for (j = 0; j < MAX_BIG2STR_TABLE_ENTRIES; ++j) {
	    big2str_power_cache[i][j] = Qnil;
	}
    }
}

static inline VALUE
power_cache_get_power0(int base, int i)
{
    if (NIL_P(big2str_power_cache[base - 2][i])) {
	big2str_power_cache[base - 2][i] =
	    i == 0 ? rb_big_pow(rb_int2big(base), INT2FIX(KARATSUBA_DIGITS))
		   : bigsqr(power_cache_get_power0(base, i - 1));
	rb_gc_register_mark_object(big2str_power_cache[base - 2][i]);
    }
    return big2str_power_cache[base - 2][i];
}

static VALUE
power_cache_get_power(int base, long n1, long* m1)
{
    long i, j, m;
    VALUE t;

    if (n1 <= KARATSUBA_DIGITS)
	rb_bug("n1 > KARATSUBA_DIGITS");

    m = ceil_log2(n1);
    if (m1) *m1 = 1 << m;
    i = m - LOG2_KARATSUBA_DIGITS;
    if (i >= MAX_BIG2STR_TABLE_ENTRIES)
	i = MAX_BIG2STR_TABLE_ENTRIES - 1;
    t = power_cache_get_power0(base, i);

    j = KARATSUBA_DIGITS*(1 << i);
    while (n1 > j) {
	t = bigsqr(t);
	j *= 2;
    }
    return t;
}

/* big2str_muraken_find_n1
 *
 * Let a natural number x is given by:
 * x = 2^0 * x_0 + 2^1 * x_1 + ... + 2^(B*n_0 - 1) * x_{B*n_0 - 1},
 * where B is BITSPERDIG (i.e. BDIGITS*CHAR_BIT) and n_0 is
 * RBIGNUM_LEN(x).
 *
 * Now, we assume n_1 = min_n \{ n | 2^(B*n_0/2) <= b_1^(n_1) \}, so
 * it is realized that 2^(B*n_0) <= {b_1}^{2*n_1}, where b_1 is a
 * given radix number. And then, we have n_1 <= (B*n_0) /
 * (2*log_2(b_1)), therefore n_1 is given by ceil((B*n_0) /
 * (2*log_2(b_1))).
 */
static long
big2str_find_n1(VALUE x, int base)
{
    static const double log_2[] = {
	1.0,              1.58496250072116, 2.0,
	2.32192809488736, 2.58496250072116, 2.8073549220576,
	3.0,              3.16992500144231, 3.32192809488736,
	3.4594316186373,  3.58496250072116, 3.70043971814109,
	3.8073549220576,  3.90689059560852, 4.0,
	4.08746284125034, 4.16992500144231, 4.24792751344359,
	4.32192809488736, 4.39231742277876, 4.4594316186373,
	4.52356195605701, 4.58496250072116, 4.64385618977472,
	4.70043971814109, 4.75488750216347, 4.8073549220576,
	4.85798099512757, 4.90689059560852, 4.95419631038688,
	5.0,              5.04439411935845, 5.08746284125034,
	5.12928301694497, 5.16992500144231
    };
    long bits;

    if (base < 2 || 36 < base)
	rb_bug("invalid radix %d", base);

    if (FIXNUM_P(x)) {
	bits = (SIZEOF_LONG*CHAR_BIT - 1)/2 + 1;
    }
    else if (BIGZEROP(x)) {
	return 0;
    }
    else if (RBIGNUM_LEN(x) >= LONG_MAX/BITSPERDIG) {
	rb_raise(rb_eRangeError, "bignum too big to convert into `string'");
    }
    else {
	bits = BITSPERDIG*RBIGNUM_LEN(x);
    }

    return (long)ceil(bits/log_2[base - 2]);
}

static long
big2str_orig(VALUE x, int base, char* ptr, long len, long hbase, int trim)
{
    long i = RBIGNUM_LEN(x), j = len;
    BDIGIT* ds = BDIGITS(x);

    while (i && j > 0) {
	long k = i;
	BDIGIT_DBL num = 0;

	while (k--) {               /* x / hbase */
	    num = BIGUP(num) + ds[k];
	    ds[k] = (BDIGIT)(num / hbase);
	    num %= hbase;
	}
	if (trim && ds[i-1] == 0) i--;
	k = SIZEOF_BDIGITS;
	while (k--) {
	    ptr[--j] = ruby_digitmap[num % base];
	    num /= base;
	    if (j <= 0) break;
	    if (trim && i == 0 && num == 0) break;
	}
    }
    if (trim) {
	while (j < len && ptr[j] == '0') j++;
	MEMMOVE(ptr, ptr + j, char, len - j);
	len -= j;
    }
    return len;
}

static long
big2str_karatsuba(VALUE x, int base, char* ptr,
		  long n1, long len, long hbase, int trim)
{
    long lh, ll, m1;
    VALUE b, q, r;

    if (BIGZEROP(x)) {
	if (trim) return 0;
	else {
	    memset(ptr, '0', len);
	    return len;
	}
    }

    if (n1 <= KARATSUBA_DIGITS) {
	return big2str_orig(x, base, ptr, len, hbase, trim);
    }

    b = power_cache_get_power(base, n1, &m1);
    bigdivmod(x, b, &q, &r);
    lh = big2str_karatsuba(q, base, ptr, (len - m1)/2,
			   len - m1, hbase, trim);
    rb_big_resize(q, 0);
    ll = big2str_karatsuba(r, base, ptr + lh, m1/2,
			   m1, hbase, !lh && trim);
    rb_big_resize(r, 0);

    return lh + ll;
}

VALUE
rb_big2str0(VALUE x, int base, int trim)
{
    int off;
    VALUE ss, xx;
    long n1, n2, len, hbase;
    char* ptr;

    if (FIXNUM_P(x)) {
	return rb_fix2str(x, base);
    }
    if (BIGZEROP(x)) {
	return rb_usascii_str_new2("0");
    }

    if (base < 2 || 36 < base)
	rb_raise(rb_eArgError, "invalid radix %d", base);

    n2 = big2str_find_n1(x, base);
    n1 = (n2 + 1) / 2;
    ss = rb_usascii_str_new(0, n2 + 1); /* plus one for sign */
    ptr = RSTRING_PTR(ss);
    ptr[0] = RBIGNUM_SIGN(x) ? '+' : '-';

    hbase = base*base;
#if SIZEOF_BDIGITS > 2
    hbase *= hbase;
#endif
    off = !(trim && RBIGNUM_SIGN(x)); /* erase plus sign if trim */
    xx = rb_big_clone(x);
    RBIGNUM_SET_SIGN(xx, 1);
    if (n1 <= KARATSUBA_DIGITS) {
	len = off + big2str_orig(xx, base, ptr + off, n2, hbase, trim);
    }
    else {
	len = off + big2str_karatsuba(xx, base, ptr + off, n1,
				      n2, hbase, trim);
    }
    rb_big_resize(xx, 0);

    ptr[len] = '\0';
    rb_str_resize(ss, len);

    return ss;
}

VALUE
rb_big2str(VALUE x, int base)
{
    return rb_big2str0(x, base, 1);
}

/*
 *  call-seq:
 *     big.to_s(base=10)   =>  string
 *
 *  Returns a string containing the representation of <i>big</i> radix
 *  <i>base</i> (2 through 36).
 *
 *     12345654321.to_s         #=> "12345654321"
 *     12345654321.to_s(2)      #=> "1011011111110110111011110000110001"
 *     12345654321.to_s(8)      #=> "133766736061"
 *     12345654321.to_s(16)     #=> "2dfdbbc31"
 *     78546939656932.to_s(36)  #=> "rubyrules"
 */

static VALUE
rb_big_to_s(int argc, VALUE *argv, VALUE x)
{
    int base;

    if (argc == 0) base = 10;
    else {
	VALUE b;

	rb_scan_args(argc, argv, "01", &b);
	base = NUM2INT(b);
    }
    return rb_big2str(x, base);
}

static VALUE
big2ulong(VALUE x, const char *type, int check)
{
    long len = RBIGNUM_LEN(x);
    BDIGIT_DBL num;
    BDIGIT *ds;

    if (len > DIGSPERLONG) {
	if (check)
	    rb_raise(rb_eRangeError, "bignum too big to convert into `%s'", type);
	len = DIGSPERLONG;
    }
    ds = BDIGITS(x);
    num = 0;
    while (len--) {
	num = BIGUP(num);
	num += ds[len];
    }
    return num;
}

VALUE
rb_big2ulong_pack(VALUE x)
{
    VALUE num = big2ulong(x, "unsigned long", Qfalse);
    if (!RBIGNUM_SIGN(x)) {
	return -num;
    }
    return num;
}

VALUE
rb_big2ulong(VALUE x)
{
    VALUE num = big2ulong(x, "unsigned long", Qtrue);

    if (!RBIGNUM_SIGN(x)) {
	if ((SIGNED_VALUE)num < 0) {
	    rb_raise(rb_eRangeError, "bignum out of range of unsigned long");
	}
	return -num;
    }
    return num;
}

SIGNED_VALUE
rb_big2long(VALUE x)
{
    VALUE num = big2ulong(x, "long", Qtrue);

    if ((SIGNED_VALUE)num < 0 &&
	(RBIGNUM_SIGN(x) || (SIGNED_VALUE)num != LONG_MIN)) {
	rb_raise(rb_eRangeError, "bignum too big to convert into `long'");
    }
    if (!RBIGNUM_SIGN(x)) return -(SIGNED_VALUE)num;
    return num;
}

#if HAVE_LONG_LONG

static unsigned LONG_LONG
big2ull(VALUE x, const char *type)
{
    long len = RBIGNUM_LEN(x);
    BDIGIT_DBL num;
    BDIGIT *ds;

    if (len > SIZEOF_LONG_LONG/SIZEOF_BDIGITS)
	rb_raise(rb_eRangeError, "bignum too big to convert into `%s'", type);
    ds = BDIGITS(x);
    num = 0;
    while (len--) {
	num = BIGUP(num);
	num += ds[len];
    }
    return num;
}

unsigned LONG_LONG
rb_big2ull(VALUE x)
{
    unsigned LONG_LONG num = big2ull(x, "unsigned long long");

    if (!RBIGNUM_SIGN(x)) return -num;
    return num;
}

LONG_LONG
rb_big2ll(VALUE x)
{
    unsigned LONG_LONG num = big2ull(x, "long long");

    if ((LONG_LONG)num < 0 && (RBIGNUM_SIGN(x)
			       || (LONG_LONG)num != LLONG_MIN)) {
	rb_raise(rb_eRangeError, "bignum too big to convert into `long long'");
    }
    if (!RBIGNUM_SIGN(x)) return -(LONG_LONG)num;
    return num;
}

#endif  /* HAVE_LONG_LONG */

static VALUE
dbl2big(double d)
{
    long i = 0;
    BDIGIT c;
    BDIGIT *digits;
    VALUE z;
    double u = (d < 0)?-d:d;

    if (isinf(d)) {
	rb_raise(rb_eFloatDomainError, d < 0 ? "-Infinity" : "Infinity");
    }
    if (isnan(d)) {
	rb_raise(rb_eFloatDomainError, "NaN");
    }

    while (!POSFIXABLE(u) || 0 != (long)u) {
	u /= (double)(BIGRAD);
	i++;
    }
    z = bignew(i, d>=0);
    digits = BDIGITS(z);
    while (i--) {
	u *= BIGRAD;
	c = (BDIGIT)u;
	u -= c;
	digits[i] = c;
    }

    return z;
}

VALUE
rb_dbl2big(double d)
{
    return bignorm(dbl2big(d));
}

static int
nlz(BDIGIT x)
{
    BDIGIT y;
    int n = BITSPERDIG;
#if BITSPERDIG > 64
    y = x >> 64; if (y) {n -= 64; x = y;}
#endif
#if BITSPERDIG > 32
    y = x >> 32; if (y) {n -= 32; x = y;}
#endif
#if BITSPERDIG > 16
    y = x >> 16; if (y) {n -= 16; x = y;}
#endif
    y = x >>  8; if (y) {n -=  8; x = y;}
    y = x >>  4; if (y) {n -=  4; x = y;}
    y = x >>  2; if (y) {n -=  2; x = y;}
    y = x >>  1; if (y) {return n - 2;}
    return n - x;
}

static double
big2dbl(VALUE x)
{
    double d = 0.0;
    long i = RBIGNUM_LEN(x), lo = 0, bits;
    BDIGIT *ds = BDIGITS(x), dl;

    if (i) {
	bits = i * BITSPERDIG - nlz(ds[i-1]);
	if (bits > DBL_MANT_DIG+DBL_MAX_EXP) {
	    d = HUGE_VAL;
	}
	else {
	    if (bits > DBL_MANT_DIG+1)
		lo = (bits -= DBL_MANT_DIG+1) / BITSPERDIG;
	    else
		bits = 0;
	    while (--i > lo) {
		d = ds[i] + BIGRAD*d;
	    }
	    dl = ds[i];
	    if (bits && (dl & (1UL << (bits %= BITSPERDIG)))) {
		int carry = dl & ~(~0UL << bits);
		if (!carry) {
		    while (i-- > 0) {
			if ((carry = ds[i]) != 0) break;
		    }
		}
		if (carry) {
		    dl &= ~0UL << bits;
		    dl += 1UL << bits;
		    if (!dl) d += 1;
		}
	    }
	    d = dl + BIGRAD*d;
	    if (lo) d = ldexp(d, lo * BITSPERDIG);
	}
    }
    if (!RBIGNUM_SIGN(x)) d = -d;
    return d;
}

double
rb_big2dbl(VALUE x)
{
    double d = big2dbl(x);

    if (isinf(d)) {
	rb_warning("Bignum out of Float range");
	if (d < 0.0)
	    d = -HUGE_VAL;
	else
	    d = HUGE_VAL;
    }
    return d;
}

/*
 *  call-seq:
 *     big.to_f -> float
 *
 *  Converts <i>big</i> to a <code>Float</code>. If <i>big</i> doesn't
 *  fit in a <code>Float</code>, the result is infinity.
 *
 */

static VALUE
rb_big_to_f(VALUE x)
{
    return DBL2NUM(rb_big2dbl(x));
}

/*
 *  call-seq:
 *     big <=> numeric   => -1, 0, +1
 *
 *  Comparison---Returns -1, 0, or +1 depending on whether <i>big</i> is
 *  less than, equal to, or greater than <i>numeric</i>. This is the
 *  basis for the tests in <code>Comparable</code>.
 *
 */

VALUE
rb_big_cmp(VALUE x, VALUE y)
{
    long xlen = RBIGNUM_LEN(x);

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      case T_FLOAT:
	{
	    double a = RFLOAT_VALUE(y);

	    if (isinf(a)) {
		if (a > 0.0) return INT2FIX(-1);
		else return INT2FIX(1);
	    }
	    return rb_dbl_cmp(rb_big2dbl(x), a);
	}

      default:
	return rb_num_coerce_cmp(x, y, rb_intern("<=>"));
    }

    if (RBIGNUM_SIGN(x) > RBIGNUM_SIGN(y)) return INT2FIX(1);
    if (RBIGNUM_SIGN(x) < RBIGNUM_SIGN(y)) return INT2FIX(-1);
    if (xlen < RBIGNUM_LEN(y))
	return (RBIGNUM_SIGN(x)) ? INT2FIX(-1) : INT2FIX(1);
    if (xlen > RBIGNUM_LEN(y))
	return (RBIGNUM_SIGN(x)) ? INT2FIX(1) : INT2FIX(-1);

    while(xlen-- && (BDIGITS(x)[xlen]==BDIGITS(y)[xlen]));
    if (-1 == xlen) return INT2FIX(0);
    return (BDIGITS(x)[xlen] > BDIGITS(y)[xlen]) ?
	(RBIGNUM_SIGN(x) ? INT2FIX(1) : INT2FIX(-1)) :
	    (RBIGNUM_SIGN(x) ? INT2FIX(-1) : INT2FIX(1));
}

/*
 *  call-seq:
 *     big == obj  => true or false
 *
 *  Returns <code>true</code> only if <i>obj</i> has the same value
 *  as <i>big</i>. Contrast this with <code>Bignum#eql?</code>, which
 *  requires <i>obj</i> to be a <code>Bignum</code>.
 *
 *     68719476736 == 68719476736.0   #=> true
 */

VALUE
rb_big_eq(VALUE x, VALUE y)
{
    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;
      case T_BIGNUM:
	break;
      case T_FLOAT:
	{
	    volatile double a, b;

	    a = RFLOAT_VALUE(y);
	    if (isnan(a)) return Qfalse;
	    b = rb_big2dbl(x);
	    return (a == b)?Qtrue:Qfalse;
	}
      default:
	return rb_equal(y, x);
    }
    if (RBIGNUM_SIGN(x) != RBIGNUM_SIGN(y)) return Qfalse;
    if (RBIGNUM_LEN(x) != RBIGNUM_LEN(y)) return Qfalse;
    if (MEMCMP(BDIGITS(x),BDIGITS(y),BDIGIT,RBIGNUM_LEN(y)) != 0) return Qfalse;
    return Qtrue;
}

/*
 *  call-seq:
 *     big.eql?(obj)   => true or false
 *
 *  Returns <code>true</code> only if <i>obj</i> is a
 *  <code>Bignum</code> with the same value as <i>big</i>. Contrast this
 *  with <code>Bignum#==</code>, which performs type conversions.
 *
 *     68719476736.eql?(68719476736.0)   #=> false
 */

static VALUE
rb_big_eql(VALUE x, VALUE y)
{
    if (TYPE(y) != T_BIGNUM) return Qfalse;
    if (RBIGNUM_SIGN(x) != RBIGNUM_SIGN(y)) return Qfalse;
    if (RBIGNUM_LEN(x) != RBIGNUM_LEN(y)) return Qfalse;
    if (MEMCMP(BDIGITS(x),BDIGITS(y),BDIGIT,RBIGNUM_LEN(y)) != 0) return Qfalse;
    return Qtrue;
}

/*
 * call-seq:
 *    -big   =>  other_big
 *
 * Unary minus (returns a new Bignum whose value is 0-big)
 */

static VALUE
rb_big_uminus(VALUE x)
{
    VALUE z = rb_big_clone(x);

    RBIGNUM_SET_SIGN(z, !RBIGNUM_SIGN(x));

    return bignorm(z);
}

/*
 * call-seq:
 *     ~big  =>  integer
 *
 * Inverts the bits in big. As Bignums are conceptually infinite
 * length, the result acts as if it had an infinite number of one
 * bits to the left. In hex representations, this is displayed
 * as two periods to the left of the digits.
 *
 *   sprintf("%X", ~0x1122334455)    #=> "..FEEDDCCBBAA"
 */

static VALUE
rb_big_neg(VALUE x)
{
    VALUE z = rb_big_clone(x);
    BDIGIT *ds;
    long i;

    if (!RBIGNUM_SIGN(x)) get2comp(z);
    ds = BDIGITS(z);
    i = RBIGNUM_LEN(x);
    if (!i) return INT2FIX(~(SIGNED_VALUE)0);
    while (i--) {
	ds[i] = ~ds[i];
    }
    RBIGNUM_SET_SIGN(z, !RBIGNUM_SIGN(z));
    if (RBIGNUM_SIGN(x)) get2comp(z);

    return bignorm(z);
}

static VALUE
bigsub(VALUE x, VALUE y)
{
    VALUE z = 0;
    BDIGIT *zds;
    BDIGIT_DBL_SIGNED num;
    long i = RBIGNUM_LEN(x);

    /* if x is larger than y, swap */
    if (RBIGNUM_LEN(x) < RBIGNUM_LEN(y)) {
	z = x; x = y; y = z;	/* swap x y */
    }
    else if (RBIGNUM_LEN(x) == RBIGNUM_LEN(y)) {
	while (i > 0) {
	    i--;
	    if (BDIGITS(x)[i] > BDIGITS(y)[i]) {
		break;
	    }
	    if (BDIGITS(x)[i] < BDIGITS(y)[i]) {
		z = x; x = y; y = z;	/* swap x y */
		break;
	    }
	}
    }

    z = bignew(RBIGNUM_LEN(x), z==0);
    zds = BDIGITS(z);

    for (i = 0, num = 0; i < RBIGNUM_LEN(y); i++) {
	num += (BDIGIT_DBL_SIGNED)BDIGITS(x)[i] - BDIGITS(y)[i];
	zds[i] = BIGLO(num);
	num = BIGDN(num);
    }
    while (num && i < RBIGNUM_LEN(x)) {
	num += BDIGITS(x)[i];
	zds[i++] = BIGLO(num);
	num = BIGDN(num);
    }
    while (i < RBIGNUM_LEN(x)) {
	zds[i] = BDIGITS(x)[i];
	i++;
    }

    return z;
}

static VALUE
bigadd(VALUE x, VALUE y, int sign)
{
    VALUE z;
    BDIGIT_DBL num;
    long i, len;

    sign = (sign == RBIGNUM_SIGN(y));
    if (RBIGNUM_SIGN(x) != sign) {
	if (sign) return bigsub(y, x);
	return bigsub(x, y);
    }

    if (RBIGNUM_LEN(x) > RBIGNUM_LEN(y)) {
	len = RBIGNUM_LEN(x) + 1;
	z = x; x = y; y = z;
    }
    else {
	len = RBIGNUM_LEN(y) + 1;
    }
    z = bignew(len, sign);

    len = RBIGNUM_LEN(x);
    for (i = 0, num = 0; i < len; i++) {
	num += (BDIGIT_DBL)BDIGITS(x)[i] + BDIGITS(y)[i];
	BDIGITS(z)[i] = BIGLO(num);
	num = BIGDN(num);
    }
    len = RBIGNUM_LEN(y);
    while (num && i < len) {
	num += BDIGITS(y)[i];
	BDIGITS(z)[i++] = BIGLO(num);
	num = BIGDN(num);
    }
    while (i < len) {
	BDIGITS(z)[i] = BDIGITS(y)[i];
	i++;
    }
    BDIGITS(z)[i] = (BDIGIT)num;

    return z;
}

/*
 *  call-seq:
 *     big + other  => Numeric
 *
 *  Adds big and other, returning the result.
 */

VALUE
rb_big_plus(VALUE x, VALUE y)
{
    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	/* fall through */
      case T_BIGNUM:
	return bignorm(bigadd(x, y, 1));

      case T_FLOAT:
	return DBL2NUM(rb_big2dbl(x) + RFLOAT_VALUE(y));

      default:
	return rb_num_coerce_bin(x, y, '+');
    }
}

/*
 *  call-seq:
 *     big - other  => Numeric
 *
 *  Subtracts other from big, returning the result.
 */

VALUE
rb_big_minus(VALUE x, VALUE y)
{
    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	/* fall through */
      case T_BIGNUM:
	return bignorm(bigadd(x, y, 0));

      case T_FLOAT:
	return DBL2NUM(rb_big2dbl(x) - RFLOAT_VALUE(y));

      default:
	return rb_num_coerce_bin(x, y, '-');
    }
}

static void
rb_big_stop(void *ptr)
{
    VALUE *stop = (VALUE*)ptr;
    *stop = Qtrue;
}

struct big_mul_struct {
    VALUE x, y, z, stop;
};

static VALUE
bigmul1(void *ptr)
{
    struct big_mul_struct *bms = (struct big_mul_struct*)ptr;
    long i, j;
    BDIGIT_DBL n = 0;
    VALUE x = bms->x, y = bms->y, z = bms->z;
    BDIGIT *zds;

    j = RBIGNUM_LEN(x) + RBIGNUM_LEN(y) + 1;
    zds = BDIGITS(z);
    while (j--) zds[j] = 0;
    for (i = 0; i < RBIGNUM_LEN(x); i++) {
	BDIGIT_DBL dd;
	if (bms->stop) return Qnil;
	dd = BDIGITS(x)[i];
	if (dd == 0) continue;
	n = 0;
	for (j = 0; j < RBIGNUM_LEN(y); j++) {
	    BDIGIT_DBL ee = n + (BDIGIT_DBL)dd * BDIGITS(y)[j];
	    n = zds[i + j] + ee;
	    if (ee) zds[i + j] = BIGLO(n);
	    n = BIGDN(n);
	}
	if (n) {
	    zds[i + j] = n;
	}
    }
    return z;
}

static VALUE
rb_big_mul0(VALUE x, VALUE y)
{
    struct big_mul_struct bms;
    volatile VALUE z;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      case T_FLOAT:
	return DBL2NUM(rb_big2dbl(x) * RFLOAT_VALUE(y));

      default:
	return rb_num_coerce_bin(x, y, '*');
    }

    bms.x = x;
    bms.y = y;
    bms.z = bignew(RBIGNUM_LEN(x) + RBIGNUM_LEN(y) + 1, RBIGNUM_SIGN(x)==RBIGNUM_SIGN(y));
    bms.stop = Qfalse;

    if (RBIGNUM_LEN(x) + RBIGNUM_LEN(y) > 10000) {
	z = rb_thread_blocking_region(bigmul1, &bms, rb_big_stop, &bms.stop);
    }
    else {
	z = bigmul1(&bms);
    }

    return z;
}

/*
 *  call-seq:
 *     big * other  => Numeric
 *
 *  Multiplies big and other, returning the result.
 */

VALUE
rb_big_mul(VALUE x, VALUE y)
{
    return bignorm(rb_big_mul0(x, y));
}

struct big_div_struct {
    long nx, ny;
    BDIGIT *yds, *zds;
    VALUE stop;
};

static VALUE
bigdivrem1(void *ptr)
{
    struct big_div_struct *bds = (struct big_div_struct*)ptr;
    long nx = bds->nx, ny = bds->ny;
    long i, j, nyzero;
    BDIGIT *yds = bds->yds, *zds = bds->zds;
    BDIGIT_DBL t2;
    BDIGIT_DBL_SIGNED num;
    BDIGIT q;

    j = nx==ny?nx+1:nx;
    for (nyzero = 0; !yds[nyzero]; nyzero++);
    do {
	if (bds->stop) return Qnil;
	if (zds[j] ==  yds[ny-1]) q = BIGRAD-1;
	else q = (BDIGIT)((BIGUP(zds[j]) + zds[j-1])/yds[ny-1]);
	if (q) {
           i = nyzero; num = 0; t2 = 0;
	    do {			/* multiply and subtract */
		BDIGIT_DBL ee;
		t2 += (BDIGIT_DBL)yds[i] * q;
		ee = num - BIGLO(t2);
		num = (BDIGIT_DBL)zds[j - ny + i] + ee;
		if (ee) zds[j - ny + i] = BIGLO(num);
		num = BIGDN(num);
		t2 = BIGDN(t2);
	    } while (++i < ny);
	    num += zds[j - ny + i] - t2;/* borrow from high digit; don't update */
	    while (num) {		/* "add back" required */
		i = 0; num = 0; q--;
		do {
		    BDIGIT_DBL ee = num + yds[i];
		    num = (BDIGIT_DBL)zds[j - ny + i] + ee;
		    if (ee) zds[j - ny + i] = BIGLO(num);
		    num = BIGDN(num);
		} while (++i < ny);
		num--;
	    }
	}
	zds[j] = q;
    } while (--j >= ny);
    return Qnil;
}

static VALUE
bigdivrem(VALUE x, VALUE y, VALUE *divp, VALUE *modp)
{
    struct big_div_struct bds;
    long nx = RBIGNUM_LEN(x), ny = RBIGNUM_LEN(y);
    long i, j;
    volatile VALUE yy, z;
    BDIGIT *xds, *yds, *zds, *tds;
    BDIGIT_DBL t2;
    BDIGIT dd, q;

    if (BIGZEROP(y)) rb_num_zerodiv();
    yds = BDIGITS(y);
    if (nx < ny || (nx == ny && BDIGITS(x)[nx - 1] < BDIGITS(y)[ny - 1])) {
	if (divp) *divp = rb_int2big(0);
	if (modp) *modp = x;
	return Qnil;
    }
    xds = BDIGITS(x);
    if (ny == 1) {
	dd = yds[0];
	z = rb_big_clone(x);
	zds = BDIGITS(z);
	t2 = 0; i = nx;
	while (i--) {
	    t2 = BIGUP(t2) + zds[i];
	    zds[i] = (BDIGIT)(t2 / dd);
	    t2 %= dd;
	}
	RBIGNUM_SET_SIGN(z, RBIGNUM_SIGN(x)==RBIGNUM_SIGN(y));
	if (modp) {
	    *modp = rb_uint2big((VALUE)t2);
	    RBIGNUM_SET_SIGN(*modp, RBIGNUM_SIGN(x));
	}
	if (divp) *divp = z;
	return Qnil;
    }
    z = bignew(nx==ny?nx+2:nx+1, RBIGNUM_SIGN(x)==RBIGNUM_SIGN(y));
    zds = BDIGITS(z);
    if (nx==ny) zds[nx+1] = 0;
    while (!yds[ny-1]) ny--;

    dd = 0;
    q = yds[ny-1];
    while ((q & (1UL<<(BITSPERDIG-1))) == 0) {
	q <<= 1UL;
	dd++;
    }
    if (dd) {
	yy = rb_big_clone(y);
	tds = BDIGITS(yy);
	j = 0;
	t2 = 0;
	while (j<ny) {
	    t2 += (BDIGIT_DBL)yds[j]<<dd;
	    tds[j++] = BIGLO(t2);
	    t2 = BIGDN(t2);
	}
	yds = tds;
	j = 0;
	t2 = 0;
	while (j<nx) {
	    t2 += (BDIGIT_DBL)xds[j]<<dd;
	    zds[j++] = BIGLO(t2);
	    t2 = BIGDN(t2);
	}
	zds[j] = (BDIGIT)t2;
    }
    else {
	zds[nx] = 0;
	j = nx;
	while (j--) zds[j] = xds[j];
    }

    bds.nx = nx;
    bds.ny = ny;
    bds.zds = zds;
    bds.yds = yds;
    bds.stop = Qfalse;
    if (RBIGNUM_LEN(x) > 10000 || RBIGNUM_LEN(y) > 10000) {
	rb_thread_blocking_region(bigdivrem1, &bds, rb_big_stop, &bds.stop);
    }
    else {
	bigdivrem1(&bds);
    }

    if (divp) {			/* move quotient down in z */
	*divp = rb_big_clone(z);
	zds = BDIGITS(*divp);
	j = (nx==ny ? nx+2 : nx+1) - ny;
	for (i = 0;i < j;i++) zds[i] = zds[i+ny];
	if (!zds[i-1]) i--;
	RBIGNUM_SET_LEN(*divp, i);
    }
    if (modp) {			/* normalize remainder */
	*modp = rb_big_clone(z);
	zds = BDIGITS(*modp);
	while (--ny && !zds[ny]); ++ny;
	if (dd) {
	    t2 = 0; i = ny;
	    while(i--) {
		t2 = (t2 | zds[i]) >> dd;
		q = zds[i];
		zds[i] = BIGLO(t2);
		t2 = BIGUP(q);
	    }
	}
	if (!zds[ny-1]) ny--;
	RBIGNUM_SET_LEN(*modp, ny);
	RBIGNUM_SET_SIGN(*modp, RBIGNUM_SIGN(x));
    }
    return z;
}

static void
bigdivmod(VALUE x, VALUE y, VALUE *divp, VALUE *modp)
{
    VALUE mod;

    bigdivrem(x, y, divp, &mod);
    if (RBIGNUM_SIGN(x) != RBIGNUM_SIGN(y) && !BIGZEROP(mod)) {
	if (divp) *divp = bigadd(*divp, rb_int2big(1), 0);
	if (modp) *modp = bigadd(mod, y, 1);
    }
    else if (modp) {
	*modp = mod;
    }
}


static VALUE
rb_big_divide(VALUE x, VALUE y, ID op)
{
    VALUE z;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      case T_FLOAT:
	{
	    double div = rb_big2dbl(x) / RFLOAT_VALUE(y);
	    if (op == '/') {
		return DBL2NUM(div);
	    }
	    else {
		return rb_dbl2big(div);
	    }
	}

      default:
	return rb_num_coerce_bin(x, y, op);
    }
    bigdivmod(x, y, &z, 0);

    return bignorm(z);
}

/*
 *  call-seq:
 *     big / other     => Numeric
 *
 *  Divides big by other, returning the result.
 */

VALUE
rb_big_div(VALUE x, VALUE y)
{
    return rb_big_divide(x, y, '/');
}

VALUE
rb_big_idiv(VALUE x, VALUE y)
{
    return rb_big_divide(x, y, rb_intern("div"));
}

/*
 *  call-seq:
 *     big % other         => Numeric
 *     big.modulo(other)   => Numeric
 *
 *  Returns big modulo other. See Numeric.divmod for more
 *  information.
 */

VALUE
rb_big_modulo(VALUE x, VALUE y)
{
    VALUE z;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      default:
	return rb_num_coerce_bin(x, y, '%');
    }
    bigdivmod(x, y, 0, &z);

    return bignorm(z);
}

/*
 *  call-seq:
 *     big.remainder(numeric)    => number
 *
 *  Returns the remainder after dividing <i>big</i> by <i>numeric</i>.
 *
 *     -1234567890987654321.remainder(13731)      #=> -6966
 *     -1234567890987654321.remainder(13731.24)   #=> -9906.22531493148
 */
static VALUE
rb_big_remainder(VALUE x, VALUE y)
{
    VALUE z;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      default:
	return rb_num_coerce_bin(x, y, rb_intern("remainder"));
    }
    bigdivrem(x, y, 0, &z);

    return bignorm(z);
}

/*
 *  call-seq:
 *     big.divmod(numeric)   => array
 *
 *  See <code>Numeric#divmod</code>.
 *
 */
VALUE
rb_big_divmod(VALUE x, VALUE y)
{
    VALUE div, mod;

    switch (TYPE(y)) {
      case T_FIXNUM:
	y = rb_int2big(FIX2LONG(y));
	break;

      case T_BIGNUM:
	break;

      default:
	return rb_num_coerce_bin(x, y, rb_intern("divmod"));
    }
    bigdivmod(x, y, &div, &mod);

    return rb_assoc_new(bignorm(div), bignorm(mod));
}

static int
bdigbitsize(BDIGIT x)
{
    int size = 1;
    int nb = BITSPERDIG / 2;
    BDIGIT bits = (~0 << nb);

    if (!x) return 0;
    while (x > 1) {
	if (x & bits) {
	    size += nb;
	    x >>= nb;
	}
	x &= ~bits;
	nb /= 2;
	bits >>= nb;
    }

    return size;
}

static VALUE big_lshift(VALUE, unsigned long);
static VALUE big_rshift(VALUE, unsigned long);

static VALUE big_shift(VALUE x, int n)
{
    if (n < 0)
	return big_lshift(x, (unsigned int)-n);
    else if (n > 0)
	return big_rshift(x, (unsigned int)n);
    return x;
}

/*
 *  call-seq:
 *     big.fdiv(numeric) -> float
 *
 *  Returns the floating point result of dividing <i>big</i> by
 *  <i>numeric</i>.
 *
 *     -1234567890987654321.fdiv(13731)      #=> -89910996357705.5
 *     -1234567890987654321.fdiv(13731.24)   #=> -89909424858035.7
 *
 */

static VALUE
rb_big_fdiv(VALUE x, VALUE y)
{
    double dx = big2dbl(x);
    double dy;

    if (isinf(dx)) {
#define DBL_BIGDIG ((DBL_MANT_DIG + BITSPERDIG) / BITSPERDIG)
	VALUE z;
	int ex, ey;

	ex = (RBIGNUM_LEN(bigtrunc(x)) - 1) * BITSPERDIG;
	ex += bdigbitsize(BDIGITS(x)[RBIGNUM_LEN(x) - 1]);
	ex -= 2 * DBL_BIGDIG * BITSPERDIG;
	if (ex) x = big_shift(x, ex);

	switch (TYPE(y)) {
	  case T_FIXNUM:
	    y = rb_int2big(FIX2LONG(y));
	  case T_BIGNUM: {
	    ey = (RBIGNUM_LEN(bigtrunc(y)) - 1) * BITSPERDIG;
	    ey += bdigbitsize(BDIGITS(y)[RBIGNUM_LEN(y) - 1]);
	    ey -= DBL_BIGDIG * BITSPERDIG;
	    if (ey) y = big_shift(y, ey);
	  bignum:
	    bigdivrem(x, y, &z, 0);
	    return DBL2NUM(ldexp(big2dbl(z), ex - ey));
	  }
	  case T_FLOAT:
	    if (isnan(RFLOAT_VALUE(y))) return y;
	    y = dbl2big(ldexp(frexp(RFLOAT_VALUE(y), &ey), DBL_MANT_DIG));
	    ey -= DBL_MANT_DIG;
	    goto bignum;
	}
    }
    switch (TYPE(y)) {
      case T_FIXNUM:
	dy = (double)FIX2LONG(y);
	break;

      case T_BIGNUM:
	dy = rb_big2dbl(y);
	break;

      case T_FLOAT:
	dy = RFLOAT_VALUE(y);
	break;

      default:
	return rb_num_coerce_bin(x, y, rb_intern("fdiv"));
    }
    return DBL2NUM(dx / dy);
}

static VALUE
bigsqr(VALUE x)
{
    long len = RBIGNUM_LEN(x), k = len / 2, i;
    VALUE a, b, a2, z;
    BDIGIT_DBL num;

    if (len < 4000 / BITSPERDIG) {
	return bigtrunc(rb_big_mul0(x, x));
    }

    a = bignew(len - k, 1);
    MEMCPY(BDIGITS(a), BDIGITS(x) + k, BDIGIT, len - k);
    b = bignew(k, 1);
    MEMCPY(BDIGITS(b), BDIGITS(x), BDIGIT, k);

    a2 = bigtrunc(bigsqr(a));
    z = bigsqr(b);
    rb_big_realloc(z, (len = 2 * k + RBIGNUM_LEN(a2)) + 1);
    while (RBIGNUM_LEN(z) < 2 * k) {
	BDIGITS(z)[RBIGNUM_LEN(z)] = 0;
	RBIGNUM_SET_LEN(z, RBIGNUM_LEN(z)+1);
    }
    MEMCPY(BDIGITS(z) + 2 * k, BDIGITS(a2), BDIGIT, RBIGNUM_LEN(a2));
    RBIGNUM_SET_LEN(z, len);
    a2 = bigtrunc(rb_big_mul0(a, b));
    len = RBIGNUM_LEN(a2);
    for (i = 0, num = 0; i < len; i++) {
	num += (BDIGIT_DBL)BDIGITS(z)[i + k] + ((BDIGIT_DBL)BDIGITS(a2)[i] << 1);
	BDIGITS(z)[i + k] = BIGLO(num);
	num = BIGDN(num);
    }
    if (num) {
	len = RBIGNUM_LEN(z);
	for (i += k; i < len && num; ++i) {
	    num += (BDIGIT_DBL)BDIGITS(z)[i];
	    BDIGITS(z)[i] = BIGLO(num);
	    num = BIGDN(num);
	}
	if (num) {
	    BDIGITS(z)[RBIGNUM_LEN(z)] = BIGLO(num);
	    RBIGNUM_SET_LEN(z, RBIGNUM_LEN(z)+1);
	}
    }
    return bigtrunc(z);
}

/*
 *  call-seq:
 *     big ** exponent   => numeric
 *
 *  Raises _big_ to the _exponent_ power (which may be an integer, float,
 *  or anything that will coerce to a number). The result may be
 *  a Fixnum, Bignum, or Float
 *
 *    123456789 ** 2      #=> 15241578750190521
 *    123456789 ** 1.2    #=> 5126464716.09932
 *    123456789 ** -2     #=> 6.5610001194102e-17
 */

VALUE
rb_big_pow(VALUE x, VALUE y)
{
    double d;
    SIGNED_VALUE yy;

    if (y == INT2FIX(0)) return INT2FIX(1);
    switch (TYPE(y)) {
      case T_FLOAT:
	d = RFLOAT_VALUE(y);
	break;

      case T_BIGNUM:
	rb_warn("in a**b, b may be too big");
	d = rb_big2dbl(y);
	break;

      case T_FIXNUM:
	yy = FIX2LONG(y);

	if (yy < 0)
	    return rb_funcall(rb_rational_raw1(x), rb_intern("**"), 1, y);
	else {
	    VALUE z = 0;
	    SIGNED_VALUE mask;
	    const long BIGLEN_LIMIT = 1024*1024 / SIZEOF_BDIGITS;

	    if ((RBIGNUM_LEN(x) > BIGLEN_LIMIT) ||
		(RBIGNUM_LEN(x) > BIGLEN_LIMIT / yy)) {
		rb_warn("in a**b, b may be too big");
		d = (double)yy;
		break;
	    }
	    for (mask = FIXNUM_MAX + 1; mask; mask >>= 1) {
		if (z) z = bigtrunc(bigsqr(z));
		if (yy & mask) {
		    z = z ? bigtrunc(rb_big_mul0(z, x)) : x;
		}
	    }
	    return bignorm(z);
	}
	/* NOTREACHED */
	break;

      default:
	return rb_num_coerce_bin(x, y, rb_intern("**"));
    }
    return DBL2NUM(pow(rb_big2dbl(x), d));
}

static VALUE
bit_coerce(VALUE x)
{
    while (!FIXNUM_P(x) && TYPE(x) != T_BIGNUM) {
	if (TYPE(x) == T_FLOAT) {
	    rb_raise(rb_eTypeError, "can't convert Float into Integer");
	}
	x = rb_to_int(x);
    }
    return x;
}

/*
 * call-seq:
 *     big & numeric   =>  integer
 *
 * Performs bitwise +and+ between _big_ and _numeric_.
 */

VALUE
rb_big_and(VALUE xx, VALUE yy)
{
    volatile VALUE x, y, z;
    BDIGIT *ds1, *ds2, *zds;
    long i, l1, l2;
    char sign;

    x = xx;
    y = bit_coerce(yy);
    if (FIXNUM_P(y)) {
	y = rb_int2big(FIX2LONG(y));
    }
    if (!RBIGNUM_SIGN(y)) {
	y = rb_big_clone(y);
	get2comp(y);
    }
    if (!RBIGNUM_SIGN(x)) {
	x = rb_big_clone(x);
	get2comp(x);
    }
    if (RBIGNUM_LEN(x) > RBIGNUM_LEN(y)) {
	l1 = RBIGNUM_LEN(y);
	l2 = RBIGNUM_LEN(x);
	ds1 = BDIGITS(y);
	ds2 = BDIGITS(x);
	sign = RBIGNUM_SIGN(y);
    }
    else {
	l1 = RBIGNUM_LEN(x);
	l2 = RBIGNUM_LEN(y);
	ds1 = BDIGITS(x);
	ds2 = BDIGITS(y);
	sign = RBIGNUM_SIGN(x);
    }
    z = bignew(l2, RBIGNUM_SIGN(x) || RBIGNUM_SIGN(y));
    zds = BDIGITS(z);

    for (i=0; i<l1; i++) {
	zds[i] = ds1[i] & ds2[i];
    }
    for (; i<l2; i++) {
	zds[i] = sign?0:ds2[i];
    }
    if (!RBIGNUM_SIGN(z)) get2comp(z);
    return bignorm(z);
}

/*
 * call-seq:
 *     big | numeric   =>  integer
 *
 * Performs bitwise +or+ between _big_ and _numeric_.
 */

VALUE
rb_big_or(VALUE xx, VALUE yy)
{
    volatile VALUE x, y, z;
    BDIGIT *ds1, *ds2, *zds;
    long i, l1, l2;
    char sign;

    x = xx;
    y = bit_coerce(yy);
    if (FIXNUM_P(y)) {
	y = rb_int2big(FIX2LONG(y));
    }

    if (!RBIGNUM_SIGN(y)) {
	y = rb_big_clone(y);
	get2comp(y);
    }
    if (!RBIGNUM_SIGN(x)) {
	x = rb_big_clone(x);
	get2comp(x);
    }
    if (RBIGNUM_LEN(x) > RBIGNUM_LEN(y)) {
	l1 = RBIGNUM_LEN(y);
	l2 = RBIGNUM_LEN(x);
	ds1 = BDIGITS(y);
	ds2 = BDIGITS(x);
	sign = RBIGNUM_SIGN(y);
    }
    else {
	l1 = RBIGNUM_LEN(x);
	l2 = RBIGNUM_LEN(y);
	ds1 = BDIGITS(x);
	ds2 = BDIGITS(y);
	sign = RBIGNUM_SIGN(x);
    }
    z = bignew(l2, RBIGNUM_SIGN(x) && RBIGNUM_SIGN(y));
    zds = BDIGITS(z);

    for (i=0; i<l1; i++) {
	zds[i] = ds1[i] | ds2[i];
    }
    for (; i<l2; i++) {
	zds[i] = sign?ds2[i]:(BIGRAD-1);
    }
    if (!RBIGNUM_SIGN(z)) get2comp(z);

    return bignorm(z);
}

/*
 * call-seq:
 *     big ^ numeric   =>  integer
 *
 * Performs bitwise +exclusive or+ between _big_ and _numeric_.
 */

VALUE
rb_big_xor(VALUE xx, VALUE yy)
{
    volatile VALUE x, y;
    VALUE z;
    BDIGIT *ds1, *ds2, *zds;
    long i, l1, l2;
    char sign;

    x = xx;
    y = bit_coerce(yy);
    if (FIXNUM_P(y)) {
	y = rb_int2big(FIX2LONG(y));
    }

    if (!RBIGNUM_SIGN(y)) {
	y = rb_big_clone(y);
	get2comp(y);
    }
    if (!RBIGNUM_SIGN(x)) {
	x = rb_big_clone(x);
	get2comp(x);
    }
    if (RBIGNUM_LEN(x) > RBIGNUM_LEN(y)) {
	l1 = RBIGNUM_LEN(y);
	l2 = RBIGNUM_LEN(x);
	ds1 = BDIGITS(y);
	ds2 = BDIGITS(x);
	sign = RBIGNUM_SIGN(y);
    }
    else {
	l1 = RBIGNUM_LEN(x);
	l2 = RBIGNUM_LEN(y);
	ds1 = BDIGITS(x);
	ds2 = BDIGITS(y);
	sign = RBIGNUM_SIGN(x);
    }
    RBIGNUM_SET_SIGN(x, RBIGNUM_SIGN(x)?1:0);
    RBIGNUM_SET_SIGN(y, RBIGNUM_SIGN(y)?1:0);
    z = bignew(l2, !(RBIGNUM_SIGN(x) ^ RBIGNUM_SIGN(y)));
    zds = BDIGITS(z);

    for (i=0; i<l1; i++) {
	zds[i] = ds1[i] ^ ds2[i];
    }
    for (; i<l2; i++) {
	zds[i] = sign?ds2[i]:~ds2[i];
    }
    if (!RBIGNUM_SIGN(z)) get2comp(z);

    return bignorm(z);
}

static VALUE
check_shiftdown(VALUE y, VALUE x)
{
    if (!RBIGNUM_LEN(x)) return INT2FIX(0);
    if (RBIGNUM_LEN(y) > SIZEOF_LONG / SIZEOF_BDIGITS) {
	return RBIGNUM_SIGN(x) ? INT2FIX(0) : INT2FIX(-1);
    }
    return Qnil;
}

/*
 * call-seq:
 *     big << numeric   =>  integer
 *
 * Shifts big left _numeric_ positions (right if _numeric_ is negative).
 */

VALUE
rb_big_lshift(VALUE x, VALUE y)
{
    long shift;
    int neg = 0;

    for (;;) {
	if (FIXNUM_P(y)) {
	    shift = FIX2LONG(y);
	    if (shift < 0) {
		neg = 1;
		shift = -shift;
	    }
	    break;
	}
	else if (TYPE(y) == T_BIGNUM) {
	    if (!RBIGNUM_SIGN(y)) {
		VALUE t = check_shiftdown(y, x);
		if (!NIL_P(t)) return t;
		neg = 1;
	    }
	    shift = big2ulong(y, "long", Qtrue);
	    break;
	}
	y = rb_to_int(y);
    }

    x = neg ? big_rshift(x, shift) : big_lshift(x, shift);
    return bignorm(x);
}

static VALUE
big_lshift(VALUE x, unsigned long shift)
{
    BDIGIT *xds, *zds;
    long s1 = shift/BITSPERDIG;
    int s2 = shift%BITSPERDIG;
    VALUE z;
    BDIGIT_DBL num = 0;
    long len, i;

    len = RBIGNUM_LEN(x);
    z = bignew(len+s1+1, RBIGNUM_SIGN(x));
    zds = BDIGITS(z);
    for (i=0; i<s1; i++) {
	*zds++ = 0;
    }
    xds = BDIGITS(x);
    for (i=0; i<len; i++) {
	num = num | (BDIGIT_DBL)*xds++<<s2;
	*zds++ = BIGLO(num);
	num = BIGDN(num);
    }
    *zds = BIGLO(num);
    return z;
}

/*
 * call-seq:
 *     big >> numeric   =>  integer
 *
 * Shifts big right _numeric_ positions (left if _numeric_ is negative).
 */

VALUE
rb_big_rshift(VALUE x, VALUE y)
{
    long shift;
    int neg = 0;

    for (;;) {
	if (FIXNUM_P(y)) {
	    shift = FIX2LONG(y);
	    if (shift < 0) {
		neg = 1;
		shift = -shift;
	    }
	    break;
	}
	else if (TYPE(y) == T_BIGNUM) {
	    if (RBIGNUM_SIGN(y)) {
		VALUE t = check_shiftdown(y, x);
		if (!NIL_P(t)) return t;
	    }
	    else {
		neg = 1;
	    }
	    shift = big2ulong(y, "long", Qtrue);
	    break;
	}
	y = rb_to_int(y);
    }

    x = neg ? big_lshift(x, shift) : big_rshift(x, shift);
    return bignorm(x);
}

static VALUE
big_rshift(VALUE x, unsigned long shift)
{
    BDIGIT *xds, *zds;
    long s1 = shift/BITSPERDIG;
    int s2 = shift%BITSPERDIG;
    VALUE z;
    BDIGIT_DBL num = 0;
    long i, j;
    volatile VALUE save_x;

    if (s1 > RBIGNUM_LEN(x)) {
	if (RBIGNUM_SIGN(x))
	    return INT2FIX(0);
	else
	    return INT2FIX(-1);
    }
    if (!RBIGNUM_SIGN(x)) {
	save_x = x = rb_big_clone(x);
	get2comp(x);
    }
    xds = BDIGITS(x);
    i = RBIGNUM_LEN(x); j = i - s1;
    if (j == 0) {
	if (RBIGNUM_SIGN(x)) return INT2FIX(0);
	else return INT2FIX(-1);
    }
    z = bignew(j, RBIGNUM_SIGN(x));
    if (!RBIGNUM_SIGN(x)) {
	num = ((BDIGIT_DBL)~0) << BITSPERDIG;
    }
    zds = BDIGITS(z);
    while (i--, j--) {
	num = (num | xds[i]) >> s2;
	zds[j] = BIGLO(num);
	num = BIGUP(xds[i]);
    }
    if (!RBIGNUM_SIGN(x)) {
	get2comp(z);
    }
    return z;
}

/*
 *  call-seq:
 *     big[n] -> 0, 1
 *
 *  Bit Reference---Returns the <em>n</em>th bit in the (assumed) binary
 *  representation of <i>big</i>, where <i>big</i>[0] is the least
 *  significant bit.
 *
 *     a = 9**15
 *     50.downto(0) do |n|
 *       print a[n]
 *     end
 *
 *  <em>produces:</em>
 *
 *     000101110110100000111000011110010100111100010111001
 *
 */

static VALUE
rb_big_aref(VALUE x, VALUE y)
{
    BDIGIT *xds;
    BDIGIT_DBL num;
    VALUE shift;
    long i, s1, s2;

    if (TYPE(y) == T_BIGNUM) {
	if (!RBIGNUM_SIGN(y))
	    return INT2FIX(0);
	if (RBIGNUM_LEN(bigtrunc(y)) > DIGSPERLONG) {
	  out_of_range:
	    return RBIGNUM_SIGN(x) ? INT2FIX(0) : INT2FIX(1);
	}
	shift = big2ulong(y, "long", Qfalse);
    }
    else {
	i = NUM2LONG(y);
	if (i < 0) return INT2FIX(0);
	shift = (VALUE)i;
    }
    s1 = shift/BITSPERDIG;
    s2 = shift%BITSPERDIG;

    if (s1 >= RBIGNUM_LEN(x)) goto out_of_range;
    if (!RBIGNUM_SIGN(x)) {
	xds = BDIGITS(x);
	i = 0; num = 1;
	while (num += ~xds[i], ++i <= s1) {
	    num = BIGDN(num);
	}
    }
    else {
	num = BDIGITS(x)[s1];
    }
    if (num & ((BDIGIT_DBL)1<<s2))
	return INT2FIX(1);
    return INT2FIX(0);
}

/*
 * call-seq:
 *   big.hash   => fixnum
 *
 * Compute a hash based on the value of _big_.
 */

static VALUE
rb_big_hash(VALUE x)
{
    int hash;

    hash = rb_memhash(BDIGITS(x), sizeof(BDIGIT)*RBIGNUM_LEN(x)) ^ RBIGNUM_SIGN(x);
    return INT2FIX(hash);
}

/*
 * MISSING: documentation
 */

static VALUE
rb_big_coerce(VALUE x, VALUE y)
{
    if (FIXNUM_P(y)) {
	return rb_assoc_new(rb_int2big(FIX2LONG(y)), x);
    }
    else if (TYPE(y) == T_BIGNUM) {
       return rb_assoc_new(y, x);
    }
    else {
	rb_raise(rb_eTypeError, "can't coerce %s to Bignum",
		 rb_obj_classname(y));
    }
    /* not reached */
    return Qnil;
}

/*
 *  call-seq:
 *     big.abs -> aBignum
 *
 *  Returns the absolute value of <i>big</i>.
 *
 *     -1234567890987654321.abs   #=> 1234567890987654321
 */

static VALUE
rb_big_abs(VALUE x)
{
    if (!RBIGNUM_SIGN(x)) {
	x = rb_big_clone(x);
	RBIGNUM_SET_SIGN(x, 1);
    }
    return x;
}

/*
 *  call-seq:
 *     big.size -> integer
 *
 *  Returns the number of bytes in the machine representation of
 *  <i>big</i>.
 *
 *     (256**10 - 1).size   #=> 12
 *     (256**20 - 1).size   #=> 20
 *     (256**40 - 1).size   #=> 40
 */

static VALUE
rb_big_size(VALUE big)
{
    return LONG2FIX(RBIGNUM_LEN(big)*SIZEOF_BDIGITS);
}

/*
 *  call-seq:
 *     big.odd? -> true or false
 *
 *  Returns <code>true</code> if <i>big</i> is an odd number.
 */

static VALUE
rb_big_odd_p(VALUE num)
{
    if (BDIGITS(num)[0] & 1) {
	return Qtrue;
    }
    return Qfalse;
}

/*
 *  call-seq:
 *     big.even? -> true or false
 *
 *  Returns <code>true</code> if <i>big</i> is an even number.
 */

static VALUE
rb_big_even_p(VALUE num)
{
    if (BDIGITS(num)[0] & 1) {
	return Qfalse;
    }
    return Qtrue;
}

/*
 *  Bignum objects hold integers outside the range of
 *  Fixnum. Bignum objects are created
 *  automatically when integer calculations would otherwise overflow a
 *  Fixnum. When a calculation involving
 *  Bignum objects returns a result that will fit in a
 *  Fixnum, the result is automatically converted.
 *
 *  For the purposes of the bitwise operations and <code>[]</code>, a
 *  Bignum is treated as if it were an infinite-length
 *  bitstring with 2's complement representation.
 *
 *  While Fixnum values are immediate, Bignum
 *  objects are not---assignment and parameter passing work with
 *  references to objects, not the objects themselves.
 *
 */

void
Init_Bignum(void)
{
    rb_cBignum = rb_define_class("Bignum", rb_cInteger);

    rb_define_method(rb_cBignum, "to_s", rb_big_to_s, -1);
    rb_define_method(rb_cBignum, "coerce", rb_big_coerce, 1);
    rb_define_method(rb_cBignum, "-@", rb_big_uminus, 0);
    rb_define_method(rb_cBignum, "+", rb_big_plus, 1);
    rb_define_method(rb_cBignum, "-", rb_big_minus, 1);
    rb_define_method(rb_cBignum, "*", rb_big_mul, 1);
    rb_define_method(rb_cBignum, "/", rb_big_div, 1);
    rb_define_method(rb_cBignum, "%", rb_big_modulo, 1);
    rb_define_method(rb_cBignum, "div", rb_big_idiv, 1);
    rb_define_method(rb_cBignum, "divmod", rb_big_divmod, 1);
    rb_define_method(rb_cBignum, "modulo", rb_big_modulo, 1);
    rb_define_method(rb_cBignum, "remainder", rb_big_remainder, 1);
    rb_define_method(rb_cBignum, "fdiv", rb_big_fdiv, 1);
    rb_define_method(rb_cBignum, "**", rb_big_pow, 1);
    rb_define_method(rb_cBignum, "&", rb_big_and, 1);
    rb_define_method(rb_cBignum, "|", rb_big_or, 1);
    rb_define_method(rb_cBignum, "^", rb_big_xor, 1);
    rb_define_method(rb_cBignum, "~", rb_big_neg, 0);
    rb_define_method(rb_cBignum, "<<", rb_big_lshift, 1);
    rb_define_method(rb_cBignum, ">>", rb_big_rshift, 1);
    rb_define_method(rb_cBignum, "[]", rb_big_aref, 1);

    rb_define_method(rb_cBignum, "<=>", rb_big_cmp, 1);
    rb_define_method(rb_cBignum, "==", rb_big_eq, 1);
    rb_define_method(rb_cBignum, "===", rb_big_eq, 1);
    rb_define_method(rb_cBignum, "eql?", rb_big_eql, 1);
    rb_define_method(rb_cBignum, "hash", rb_big_hash, 0);
    rb_define_method(rb_cBignum, "to_f", rb_big_to_f, 0);
    rb_define_method(rb_cBignum, "abs", rb_big_abs, 0);
    rb_define_method(rb_cBignum, "magnitude", rb_big_abs, 0);
    rb_define_method(rb_cBignum, "size", rb_big_size, 0);
    rb_define_method(rb_cBignum, "odd?", rb_big_odd_p, 0);
    rb_define_method(rb_cBignum, "even?", rb_big_even_p, 0);

    power_cache_init();
}