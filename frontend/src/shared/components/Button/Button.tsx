import { FC } from 'react';
import { getClassName } from 'src/shared/helpers/getClassName';

import utilClasses from '../../styles/util.module.css';

import { IProps } from './interface';
import classes from './style.module.css';

const Button: FC<IProps> = ({
  children,
  appearance = 'primaryFilled',
  status = 'idle',
  onClick = () => {},
}) => {
  const buttonClassName = getClassName([
    classes.button,
    classes[`button_appearance_${appearance}`],
  ]);

  return (
    <button className={buttonClassName} type="button" onClick={onClick}>
      {status !== 'loading' && children}
      {status === 'loading' && (
        <div className={utilClasses.loader}>
          <Loader />
        </div>
      )}
    </button>
  );
};

function Loader() {
  return (
    <svg width={15} height={15} fill="none" xmlns="http://www.w3.org/2000/svg">
      <circle cx={7.5} cy={7.5} r={7} stroke="var(--btn-typo-clr)" />
      <mask
        id="a"
        style={{
          maskType: 'alpha',
        }}
        maskUnits="userSpaceOnUse"
        x={0}
        y={7}
        width={8}
        height={8}
      >
        <path d="M0 7.5h7.5V15H0V7.5Z" fill="var(--btn-typo-clr)" />
      </mask>
      <g mask="url(#a)">
        <circle cx={7.5} cy={7.5} r={7} stroke="var(--btn-bg-clr)" />
      </g>
      <mask
        id="b"
        style={{
          maskType: 'alpha',
        }}
        maskUnits="userSpaceOnUse"
        x={7}
        y={0}
        width={8}
        height={8}
      >
        <path d="M7.5 0H15v7.5H7.5V0Z" fill="var(--btn-typo-clr)" />
      </mask>
      <g mask="url(#b)">
        <circle cx={7.5} cy={7.5} r={7} stroke="var(--btn-bg-clr)" />
      </g>
    </svg>
  );
}

export { Button };
