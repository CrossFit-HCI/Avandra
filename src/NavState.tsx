import { createSlice } from '@reduxjs/toolkit';
import { TypedUseSelectorHook, useDispatch, useSelector } from 'react-redux';
import { AppDispatch, RootState } from './Store';

interface NavOpened {
    status: 'opened';
}
  
interface NavClosed {
    status: 'closed';
}

type NavStatus = NavOpened | NavClosed;

const navOpened: NavStatus = {status: 'opened'}
const navClosed: NavStatus = {status: 'closed'}

interface NavState {
    navStatus: NavStatus
}

const initialNavState: NavState = {
    navStatus: navOpened
}

const navSlice = createSlice({
    name: 'nav',
    initialState: initialNavState,
    reducers: {
        openNav: (state) => {
            state.navStatus = navOpened;
        },
        closeNav: (state) => {
            state.navStatus = navClosed;
        },
    },
});

export const {openNav, closeNav } = navSlice.actions;

export const useAppDispatch: () => AppDispatch = useDispatch
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector

export default navSlice.reducer;

