import { ChangeEvent, FC, PropsWithChildren, useEffect, useState } from 'react';
import { getClassName } from 'src/shared/helpers/getClassName';

import utilClasses from '../../styles/util.module.css';
import { TextLink } from '../TextLink';

import { IItem, IProps } from './interface';
import classes from './style.module.css';

const Input: FC<IProps> = ({
  value,
  label,
  status,
  message,
  items,
  button,
  onChange,
  onSelect,
  onClick,
}) => {
  const { editMode, handleInputFocus, handleInputBlur, handleInputChange } =
    useInput(value, onChange);
  const { selectMode, handleSelectItem, handleSelectBlur, handleSelectFocus } =
    useSelect(items, onChange, onSelect);

  const inputClassName = getClassName([
    classes.input,
    [editMode, classes.input_editMode],
    [status === 'failed', classes.input_error]
  ]);

  const handleFocus = () => {
    handleInputFocus();
    handleSelectFocus();
  };

  const handleBlur = () => {
    handleInputBlur();
    handleSelectBlur();
  };

  return (
    <label className={inputClassName}>
      <span className={classes.label}>{label}</span>
      <input
        className={classes.field}
        value={value}
        onChange={handleInputChange}
        onFocus={handleFocus}
        onBlur={handleBlur}
      />

      {status == 'loading' && (
        <span className={classes.status}>
          <span className={utilClasses.loader}>
            <Loader />
          </span>
        </span>
      )}

      {status !== 'loading' && button && (
        <button className={classes.button} type="button" onClick={onClick}>
          <TextLink appearance="secondary">{button}</TextLink>
        </button>
      )}

      {message && (
        <span className={classes.message}>{message}</span>
      )}

      {items && selectMode && (
        <ul className={classes.select}>
          {items.map((item) => (
            <Item key={item.id} {...item} onSelect={handleSelectItem} />
          ))}
        </ul>
      )}
    </label>
  );
};

function Item({
  id,
  value,
  onSelect,
}: PropsWithChildren<IItem & { onSelect(item: IItem): void }>) {
  const handleClick = () => {
    onSelect({ id, value });
  };

  return (
    <li className={classes.item} onClick={handleClick}>
      {value}
    </li>
  );
}

function Loader() {
  return (
    <svg width={15} height={15} fill="none" xmlns="http://www.w3.org/2000/svg">
      <circle cx={7.5} cy={7.5} r={7} stroke="#DCDCDC" />
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
        <path d="M0 7.5h7.5V15H0V7.5Z" fill="#C4C4C4" />
      </mask>
      <g mask="url(#a)">
        <circle cx={7.5} cy={7.5} r={7} stroke="#020203" />
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
        <path d="M7.5 0H15v7.5H7.5V0Z" fill="#C4C4C4" />
      </mask>
      <g mask="url(#b)">
        <circle cx={7.5} cy={7.5} r={7} stroke="#020203" />
      </g>
    </svg>
  );
}

function useInput(value: string, onChange?: (val: string) => void) {
  const [editMode, setEditMode] = useState(false);
  const [focused, setFocused] = useState(false);

  const notEmpty = value.length > 0;

  useEffect(() => {
    if (!focused) setEditMode(notEmpty);
  }, [notEmpty]);

  const handleInputFocus = () => {
    if (!notEmpty) setEditMode(true);
    setFocused(true);
  };

  const handleInputBlur = () => {
    if (!notEmpty) setEditMode(false);
    setFocused(false);
  };

  const handleInputChange = (evt: ChangeEvent<HTMLInputElement>) => {
    onChange?.call(undefined, evt.currentTarget.value);
  };

  return { editMode, handleInputFocus, handleInputBlur, handleInputChange };
}

function useSelect(
  items?: IItem[],
  onChange?: (val: string) => void,
  onSelect?: (id: string) => void,
) {
  const [selectMode, setSelectMode] = useState(false);

  const isNotEmpty = !!items?.length;

  useEffect(() => {
    if (!isNotEmpty) setSelectMode(isNotEmpty);
  }, [isNotEmpty]);

  const handleSelectItem = ({ id, value }: { id: string; value: string }) => {
    setSelectMode(false);
    onSelect?.call(undefined, id);
    onChange?.call(undefined, value);
  };

  const handleSelectFocus = () => {
    setSelectMode(true);
  };

  const handleSelectBlur = () => {
    setSelectMode(false);
  };

  return { selectMode, handleSelectItem, handleSelectFocus, handleSelectBlur };
}

export { Input };
