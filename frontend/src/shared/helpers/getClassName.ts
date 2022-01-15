const concat = (result: string, str: string) => result + ' ' + str;

export function getClassName(
  xs: (string | [cond: boolean | undefined | null, className: string])[],
) {
  return xs.reduce((result: string, x) => {
    if (typeof x === 'string') return concat(result, x);
    if (x[0]) return concat(result, x[1]);

    return result;
  }, '');
}
