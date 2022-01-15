import type { AppProps } from 'next/app';
import 'the-new-css-reset';

import 'src/shared/styles/fonts.css';
import 'src/shared/styles/variables.css';

function MyApp({ Component, pageProps }: AppProps) {
  return <Component {...pageProps} />;
}

export default MyApp;
