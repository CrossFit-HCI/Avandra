import { createSlice } from "@reduxjs/toolkit";
import { AppDispatch, RootState } from "./NavStore";
import { TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";
import { ReactNode, createContext } from "react";

interface NavOpened {
    status: 'opened';
}
  
interface NavClosed {
    status: 'closed';
}

export type NavStatus = NavOpened | NavClosed;

export const navOpened: NavStatus = {status: 'opened'}
export const navClosed: NavStatus = {status: 'closed'}

export interface NavScreenState {
    id: string,
    view: ReactNode
}

interface NavContext {
    screens: NavScreenState[]
}

export const initialNavContext: NavContext = {
    screens: []
}

// Setup the screens context with just `children` in it.
export const NavContext = createContext(initialNavContext);

interface NavState {
    navStatus: NavStatus,    
    screensPtr: number,
    screensLength: number    
}

const initialNavState: NavState = {
    navStatus: navClosed,
    screensPtr: 0,
    screensLength: 0
}

const navSlice = createSlice({
    name: 'navStatus',
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
        },  
        setMainView: (state) => {
            state.screensLength = 1;
        },
        /**
         * Transitions the Nav's main view to the given screen.
         * @param state - the current state (NavState) of the Nav.
         * @param payload - A NavScreen to inject into the Nav.
         * @returns - the update state (NavState).
         */
        injectScreens(state, payload) {
            const screensLength: number = payload.payload;
            state.screensPtr = 1;
            state.screensLength = screensLength;

            return state;            
        },
        /**
         * Resets the Nav view back to the main view.  Then cleans up the state.
         * @param state - The current state (NavState) of the Nav.
         * @returns - The updated state (NavState).
         */
        ejectScreens(state) {            
            state.screensPtr = 0;
            state.screensLength = 0;

            return state;
        },
        /**
         * Transition to the next screen on the stack. This is similar to
         * popping the stack.
         * @param state - The current state (NavState) of the Nav.
         * @returns - The updated state (NavState).
         */
        popTransition(state) {
            let screensPtr = state.screensPtr;
            let stackLength = state.screensLength - 1;  

            if (screensPtr < stackLength) {
                state.screensPtr = screensPtr + 1;                    
            } else {
                // When we reach the end of the stack, we wrap around to the
                // beginning of the stack.
                state.screensPtr = 1;
            }

            return state;
        },
        /**
         * Transition back to the previous screen on the stack.  
         * @param state - The current state (NavState) of the Nav.
         * @returns - The updated state (NavState).
         */
        goBack(state) {
            let screensPtr = state.screensPtr; 

            if (screensPtr > 0) {
                state.screensPtr = screensPtr - 1;                    
            } else {
                // When we reach the beginning of the stack, we wrap around
                // to the end of the stack.
                let screensLength =  state.screensLength - 1;
                state.screensPtr = screensLength;
            } 

            return state;
        }
    },    
});

export const {openNav, closeNav, toggleNav, setMainView, injectScreens, popTransition } = navSlice.actions;

export const useAppDispatch: () => AppDispatch = useDispatch
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector

export default navSlice.reducer;
