#include <stdlib.h>
#include <stdint.h>



size_t break_c_8(const char *s)
{
  const char *p = s;
  while (1)
  {
    const char x = *p;
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s);
    p++;
  }
}


size_t break_c_32(const char *s)
{
  const char *p = s;
  while (1)
  {
    uint32_t w = *( (uint32_t *) p );
    char x;

    x = (char) w;
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s);

    x = (char) (w >> 8);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 1);

    x = (char) (w >> 16);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 2);

    x = (char) (w >> 24);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 3);

    p += 4;
  }
}


size_t break_c_64(const char *s)
{
  const char *p = s;
  while (1)
  {
    uint64_t w = *( (uint64_t *) p );
    char x;

    x = (char) w;
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s);

    x = (char) (w >> 8);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 1);

    x = (char) (w >> 16);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 2);

    x = (char) (w >> 24);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 3);

    x = (char) (w >> 32);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 4);

    x = (char) (w >> 40);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 5);

    x = (char) (w >> 48);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 6);

    x = (char) (w >> 56);
    if (x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0'))
      return (p - s + 7);

    p += 8;
  }
}



size_t memchr_simple(const char *s, char c)
{
  const char *p = s;
  while (1)
  {
    const char x = *p;
    if (x == c) return (p - s);
    p++;
  }
}


size_t memchr_nl1(const char *s)
{
  const char *p = s;
  while (1)
  {
    const char x = *p;
    if (x == '\n') return (p - s);
    p++;
  }
}

size_t memchr_nl2(const char *s)
{
  const char *p = s;
  while (1)
  {
    const char x = *p;
    if (x <= '$' && (x == '\n' || x == '\0')) return (p - s);
    p++;
  }
}

size_t memchr_nl3(const char *s)
{
  const char *p = s;
  while (1)
  {
    const char x = *p;
    if (x == '\n' || x == '\0') return (p - s);
    p++;
  }
}

size_t memchr_nl4(const char *s)
{
  const char *p = s;
  while (1)
  {
    const char x = *p;
    if (x <= '$' && (x == '\n')) return (p - s);
    p++;
  }
}
