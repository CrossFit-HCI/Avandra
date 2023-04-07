import { createSlice } from "@reduxjs/toolkit";
import { AppDispatch, RootState } from "./NavStore";
import { TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";

interface NavOpened {
    status: 'opened';
}
  
interface NavClosed {
    status: 'closed';
}

export type NavStatus = NavOpened | NavClosed;

export const navOpened: NavStatus = {status: 'opened'}
export const navClosed: NavStatus = {status: 'closed'}

interface NavState {
    navStatus: NavStatus
}

const initialNavState: NavState = {
    navStatus: navClosed
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
        toggleNav: (state) => {
            if (state.navStatus.status == 'opened') {
                state.navStatus = navClosed;
            } else {
                state.navStatus = navOpened;
            }
        }
    },
});

export const {openNav, closeNav, toggleNav } = navSlice.actions;

export const useAppDispatch: () => AppDispatch = useDispatch
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector

export default navSlice.reducer;
