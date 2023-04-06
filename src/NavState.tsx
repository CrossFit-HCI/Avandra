interface NavOpened {
    status: 'opened';
}
  
interface NavClosed {
    status: 'closed';
}

export type NavStatus = NavOpened | NavClosed;

export const navOpened: NavStatus = {status: 'opened'}
export const navClosed: NavStatus = {status: 'closed'}
