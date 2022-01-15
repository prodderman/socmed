import { FC } from 'react';
import { getClassName } from 'src/shared/helpers/getClassName';

import { IProps } from './interface';
import classes from './style.module.css';

const TextLink: FC<IProps> = ({ children, appearance }) => {
  const textLinkClassName = getClassName([
    classes.textLink,
    classes[`textLink_appearance_${appearance}`],
  ]);

  return <span className={textLinkClassName}>{children}</span>;
};

export { TextLink };
