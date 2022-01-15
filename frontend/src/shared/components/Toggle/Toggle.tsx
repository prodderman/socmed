import { ChangeEvent, FC } from 'react';
import { getClassName } from 'src/shared/helpers/getClassName';

import { IProps } from './interface';
import classes from './style.module.css';

const Toggle: FC<IProps> = ({ checked, children, onChange }) => {
  const toggleClassName = getClassName([
    classes.toggle,
    [checked, classes.toggle_checked]
  ]);

  const handleChange = (evt: ChangeEvent<HTMLInputElement>) => {
    onChange?.call(undefined, evt.currentTarget.checked);
  };

  return (
    <label className={toggleClassName}>
      <input
        className={classes.input}
        type="checkbox"
        checked={checked}
        onChange={handleChange}
      />
      <div className={classes.marker}>
        <div className={classes.icon} />
      </div>
      {children}
    </label>
  );
};

export { Toggle };
