interface IProps {
  children: string;
  appearance?: 'primaryFilled' | 'primaryShaped' | 'secondary' | 'transparent';
  status?: TStatus;
  onClick?(): void;
}

export type { IProps };
