interface IItem {
  id: string;
  value: string;
}

interface IProps {
  value: string;
  label: string;
  status?: TStatus;
  message?: string;
  items?: IItem[];
  button?: string;
  onChange?(value: string): void;
  onSelect?(id: string): void;
  onClick?(): void;
}

export type { IItem, IProps };
