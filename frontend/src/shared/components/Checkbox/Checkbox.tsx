import { ChangeEvent, FC } from 'react';
import { getClassName } from 'src/shared/helpers/getClassName';

import { IProps } from './interface';
import classes from './style.module.css';

const Checkbox: FC<IProps> = ({ children, checked, onChange }) => {
  const checkboxClassName = getClassName([
    classes.checkbox,
    [checked, classes.checkbox_checked]
  ]);

  const handleChange = (evt: ChangeEvent<HTMLInputElement>) => {
    onChange?.call(undefined, evt.currentTarget.checked);
  };

  return (
    <label className={checkboxClassName}>
      <input
        className={classes.input}
        type="checkbox"
        checked={checked}
        onChange={handleChange}
      />
      <div className={classes.marker}>
        <CheckedIcon />
      </div>
      {children}
    </label>
  );
};

function CheckedIcon() {
  return (
    <svg width={9} height={8} fill="none" xmlns="http://www.w3.org/2000/svg">
      <path
        fillRule="evenodd"
        clipRule="evenodd"
        d="M8.416 1.278a.5.5 0 0 0-.832-.555L3.922 6.216l-2.569-2.57a.5.5 0 1 0-.707.708L3.64 7.347a.5.5 0 0 0 .783-.078l3.994-5.991Z"
        fill="var(--checkbox-icon-clr)"
      />
    </svg>
  );
}

export { Checkbox };
