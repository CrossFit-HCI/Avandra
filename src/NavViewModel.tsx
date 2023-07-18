import { configureStore, createSlice } from "@reduxjs/toolkit";
import { Provider, TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";
import React, { Children, JSXElementConstructor, ReactElement, ReactNode, createContext } from "react";
import { Button, GestureResponderEvent, View } from "react-native";

import Maybe, { just, nothing } from "./Maybe";
import { BinTree, empty, insert, lookup, mkKeyString } from "./Map";

interface NavOpened {
    status: 'opened';
}
  
interface NavClosed {
    status: 'closed';
}

type NavStatus = NavOpened | NavClosed;

export const navOpened: NavStatus = {status: 'opened'}
export const navClosed: NavStatus = {status: 'closed'}

interface ContextLabel {
    index: number,
    label: string,
    type: "ContextLabel"
}

const mkContextLabel = (index: number, label: string): ContextLabel => {
    return {index: index, label: label, type: "ContextLabel"};
}

interface Screen {
    label: string,
    screen: ReactElement
}

interface Stack {
    stack: Screen[]
}

const mainScreenLabel: string = '__main__';
const mainScreenIndex: number = -1;

/** The Nav's context. It contains the screens of the Nav. */
export interface NavContext {
    /** The Nav's main screen. */
    mainScreen: ReactElement,
    /** The stack of screens that can be injected into the Nav. */
    screens: Stack
}

/**
 * Finds the index of the corresponding modal with identifier `id`.
 * 
 * @param label The identifier of the modal.
 * @returns The `index` for the corresponding modal, or nothing.
 */
const findScreen = (context: NavContext, label: string) => {
    return context.screens.stack.findIndex((value) => {
        return value.label == label
    });
}

export interface NavScreenProps {    
    label: string,
    screen: ReactElement
}

export const initialNavContext: NavContext = Object.freeze({
    mainScreen: (<View></View>),
    screens: {stack: []},
    modals: []
});

const extractScreens = (acc:Screen[], child: ReactNode): Screen[] => {           
    // We have to cast child.type, because NavScreen is a function component.         
    if (React.isValidElement(child) && typeof child.type != 'string') {
        let component: JSXElementConstructor<any> = child.type;
        // Make sure we only have NavScreen's in the group:
        if (component.name == 'NavScreen') {
            // Now the child.props should be a NavScreenProps:
            acc.push(child.props);
            return acc;
        } else {
            throw new Error ('A NavScreen is the only component allowable in a Nav group.')  
        }      
    } else {
        throw new Error ('extractGroup: child is not a valid React element.')
    }          
}

/**
 * Get the screens from the children of a Nav component.
 * 
 * @param children The children from `NavProps` to extract the screens from.
 * @returns A `NavContext`
 */
export const createNavContext = (main: ReactElement, children: ReactNode): NavContext => {
    if (children === undefined) {
        return {mainScreen: main, screens: {stack: []}};
    } else {
        const screensArray: Screen[] = Children.toArray(children).reduce<Screen[]>(extractScreens, []);

        return {
            mainScreen: main,
            screens: {stack: screensArray}
        };
    }
}

interface NavState {
    /** Used to toggle the Nav from opened to closed. */
    navStatus: NavStatus,    

    /** Determines which screen should be shown in the Nav. */
    currentScreen: Maybe<ContextLabel>,

    /** The screen we left when showing a modal or a stack. */
    previousScreen: Maybe<ContextLabel>,
    
    /**
     * Bimap between stack screen id's and array indices into the Context of
     * stack screens. 
     */
    screenMap: BinTree<string, number>
}

/** The initial state of the Nav. 
 * 
 * Initially, the Nav starts:
 * - closed, and
 * - showing the `NavGroupType.MainScreen`.
*/
const initialNavState: NavState = {
    /** The Nav always starts out closed. */
    navStatus: navClosed,
    /** Always start out at the main screen. */
    currentScreen: just (mkContextLabel(mainScreenIndex, mainScreenLabel)),
    previousScreen: nothing,    
    screenMap: empty()
}

/**
 * A selector for determining if the Nav is opened.
 * @param state a Redux state containing the `NavState`.
 * @returns `true` if the Nav's status is set to open, and `false` otherwise.
 */
export const isNavOpenedSelector = (state:{nav: NavState}) => {
    return state.nav.navStatus == navOpened
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
        injectMainView: (state) => {
            state.currentScreen = just(mkContextLabel(mainScreenIndex, mainScreenLabel));
        },       
        /**
         * Sets the current screen to be the previous screen before opening a
         * modal or stack.
         */
        ejectScreen: (state) => {
            switch (state.previousScreen.type) {
                case ("just"): 
                    state.currentScreen = just(state.previousScreen.value);
                    state.previousScreen = nothing;
                case ("nothing"): 
                    return;
            }            
        },
        /**
         * Injects a modal into the Nav.
         * @param payload The label of the modal.
         */
        injectScreen: (state, {payload}) => {
            let label: string = payload;
            let screenIndexM: Maybe<number> = lookup(mkKeyString(label), state.screenMap);

            switch(screenIndexM.type) {
                case "just":
                    switch (state.currentScreen.type) {                        
                        case "just":
                            if (state.currentScreen.value.label != label) {
                                state.previousScreen = state.currentScreen;
                                state.currentScreen = just(mkContextLabel(screenIndexM.value, label));
                            }

                            return state;
                        case "nothing":
                            state.previousScreen = nothing;
                            state.currentScreen = just(mkContextLabel(screenIndexM.value, label));
                            return
                    }           
                case "nothing":
                    return
            }
        },       
        /**
         * Link the modal screens in the context with the state.
         * @param payload A `NavGroupState` containing the modal screens.
         */
        linkScreens: (state, action) => {
            let key: string = action.payload.key;
            let value: number = action.payload.value;

            state.screenMap = insert(mkKeyString(key), value, state.screenMap);

            return state;
        }
    },    
});

export const { openNav, closeNav, toggleNav, injectMainView, injectScreen: injectScreen, ejectScreen, linkScreens } = navSlice.actions;
export const useAppDispatch: () => AppDispatch = useDispatch
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector

const store = configureStore({ 
    reducer: {
        nav: navSlice.reducer,
    }, 
});

type RootState = ReturnType<typeof store.getState>;
type AppDispatch = typeof store.dispatch;

export const currentScreenSelector = (state:RootState) => {
    return state.nav.currentScreen
}

export const getNavScreen = (context: NavContext, state: RootState): ReactNode => {            
    const currentScreen = currentScreenSelector(state);  

    switch (currentScreen.type) {
        case ("just"): {
            if (currentScreen.value.label == mainScreenLabel && currentScreen.value.index == mainScreenIndex) {
                return context.mainScreen;
            } else {
                return context.screens.stack[currentScreen.value.index].screen;
            }
        }        
    }
}

/****************************************************
 *  Components for utilizing the store and context. *
 ****************************************************/

/**
 * The type of props for Nav buttons.
 */
interface NavButtonProps {
    /** The title of the button. */
    title: string;
    /** The call back for when the button is pressed. */
    onPress: (event: GestureResponderEvent) => void;
}

/**
 * A button for navigating to a new screen.
 * @param props - A `NavButtonProps`.
 * @returns A RN Button with the onPress callback wrapped to manage the state of
 * the Nav properly.
 */
export const NaviButton = (props:NavButtonProps) => {
    const dispatch = useAppDispatch();
    const closeTheNav = () => dispatch(closeNav())
    
    const onPressCallback = (event: GestureResponderEvent) => {
        // Make sure the Nav is closed before transitioning to a new screen.
        closeTheNav();
        props.onPress(event);
    }

    return (
        <Button title={props.title} onPress={onPressCallback} />
    )
}
interface OpenModalProps {
    title: string,
    label: string
}

export const OpenScreen = (props:OpenModalProps) => {
    const dispatch = useAppDispatch();

    const onPressCallback = (event: GestureResponderEvent) => {
        // Switch the current Nav screen to the modal with label.
        dispatch(injectScreen(props.label));
    };

    return (
        <Button title={props.title} onPress={onPressCallback} />
    );
}

export const CloseScreen = (props:{title: string}) => {
    const dispatch = useAppDispatch();

    const onPressCallback = (event: GestureResponderEvent) => {
        // Switch the current Nav screen to the modal with label.
        dispatch(ejectScreen());
    };

    return (
        <Button title={props.title} onPress={onPressCallback} />
    );
}

export interface NavProviderProps {
    children: ReactNode;
}

/**
 * The Nav Provider.
 */
export default ({children} : NavProviderProps) => {
    return (
        <Provider store={store}>
            {children}            
        </Provider>
    )
}
