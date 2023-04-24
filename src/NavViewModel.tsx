import { configureStore, createSlice } from "@reduxjs/toolkit";
import { Provider, TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";
import { Children, ReactNode, createContext, useContext } from "react";

import { Maybe, just, nothing } from "./Maybe";
import React from "react";

interface NavOpened {
    status: 'opened';
}
  
interface NavClosed {
    status: 'closed';
}

type NavStatus = NavOpened | NavClosed;

const navOpened: NavStatus = {status: 'opened'}
const navClosed: NavStatus = {status: 'closed'}

/** The state of a Nav screen. */
export interface NavScreenProps {
    /** Some identifier for the screen. */
    id: string,
    /** The screens view. */
    screen: ReactNode
}

/** The types of screens the Nav supports. */
enum NavGroupType {
    /** The main screen of the Nav. */
    MainScreen = 'MainScreen',
    /** A stack of Nav screens. */
    NavStack = 'NavStack',
    /** Modals that can be injected into the Nav. */
    NavModal = 'NavModals'
}

/** A generic datatype for describing groups of Nav screens. */
interface NavGroupState {
    /** The type of screen being described. */
    type: NavGroupType,
    /** An identifier for the screen. */
    id: string,
    /** An array of screens for the group. */
    group: NavScreenProps[]
}

/** The Nav's context. It contains the screens of the Nav. */
export interface NavContext {
    /** The Nav's main screen. */
    mainScreen: NavScreenProps,
    /** The stacks that can be injected into the Nav. */
    stacks: NavGroupState[],
    /** The modals that can be injected into the Nav. */
    modals: NavGroupState
}

const initialNavContext: NavContext = {
    mainScreen: {id: NavGroupType.MainScreen, screen: null},
    stacks: [],
    modals: {type: NavGroupType.NavModal, id: '', group: []}
}

/**
 * Finds the index of the corresponding screen group with identifier `id`.
 * 
 * @param id The identifier of the group.
 * @returns The `index` for the corresponding group, or nothing.
 */
const findIndexInGroup = (group: NavGroupType, id: string) => {
    let context = useContext(NavContext);
    let index = -1;
    
    switch (group) {
        case NavGroupType.MainScreen:
            return nothing;
        case NavGroupType.NavStack:
            context.stacks.findIndex((value) => {
                return value.id == id;
            });
        case NavGroupType.NavStack:
            context.modals.group.findIndex((value) => {
                return value.id == id;
            });

    }            

    return ((index >= 0) ? just(index) : nothing);
}

/**
 * Finds the index of the corresponding stack group with identifier `id`.
 * 
 * @param id The identifier of the stack.
 * @returns The `index` for the corresponding stack, or nothing.
 */
const findStack = (id: string) => {
    return findIndexInGroup(NavGroupType.NavStack, id);
}

/**
 * Finds the index of the corresponding modal with identifier `id`.
 * 
 * @param id The identifier of the modal.
 * @returns The `index` for the corresponding modal, or nothing.
 */
const findModal = (id: string) => {
    return findIndexInGroup(NavGroupType.NavModal, id);
}

/** 
 * Sets up the screens context with just `children` in it. */
const NavContext = createContext(initialNavContext);

const extractGroup = (children: ReactNode) => {
    return Children.toArray(children).reduce<NavScreenProps[]>((acc, child) => {
      if (React.isValidElement(child)) {
        // Make sure we only have NavScreen's in the group:
        if (child.type == 'NavScreen') {
          // Now the child.props should be a NavScreenState:
          acc.push(child.props);
          return acc;
        } else {
          throw new Error ('A NavScreen is the only component allowable in a Nav group.')  
        }      
      } else {
        throw new Error ('extractGroup: child is not a valid React element.')
      }    
    }, [])
}
  
const extractScreens = (acc:NavContext, child: ReactNode) => {    
    if (React.isValidElement(child)) {
        // 1. Make sure that we are in a NavScreen component:
        if (child.type == 'NavScreens') {        
        // 2: child should have it's own children:      
        const screens = Children.toArray(child.props.children).reduce<NavContext>((acc, childScreen) => {
            // 3. Pattern match over the possible screen groups:
            switch (child.type) {
            case NavGroupType.MainScreen:
                // 3.a. Extract the main screen:
                acc.mainScreen = child.props.screen;
                return acc;
                
            case NavGroupType.NavStack:
                // 3.b: Extract the stack screens:
                const stackScreens = extractGroup(child.props.children);
                const newStack: NavGroupState = {
                    type: NavGroupType.NavStack, 
                    id: child.props.id, 
                    group: stackScreens
                };
                acc.stacks.push(newStack);
                return acc;

            case NavGroupType.NavModal:
                // 3.c: Extract the modal screens:
                const modalScreens = extractGroup(child.props.children);
                const newModals: NavGroupState = {
                    type: NavGroupType.NavModal, 
                    id: child.props.id, 
                    group: modalScreens
                };
                acc.modals = newModals;
                return acc;

            default:
                throw new Error (`A component in NavScreens has the wrong type: ${child.type}.\n 
                NavScreens can only contain MainScreen, NavStack, or NavModals components as children.`)
            }        
        }, initialNavContext)      

        return screens;
        } else {
        throw new Error (`A component in the Nav has the wrong type: ${child.type}.\n
            Only NavScreen can be used in the Nav.`)
        }      
    } else {
        throw new Error ('A NavScreen is the only component allowable in the Nav.')
    }    
}

/**
 * Get the screens from the children of a NavScreens component.
 * 
 * @param children The children from `NavProps` to extract the screens from.
 * @returns A `NavContext`
 */
export const getScreens = (children: ReactNode) => {
    return Children.toArray(children).reduce<NavContext>(extractScreens, initialNavContext);
}

interface NavState {
    /** Used to toggle the Nav from opened to closed. */
    navStatus: NavStatus,    

    /** Determines which screen should be shown in the Nav. */
    screenGroup: NavGroupType,
    /** If `screenGroup = NavStack`, then this contains the index to the stack
     * in the `stacks` array in the context. */
    stackPtr?: number,
    /** If `screenGroup = NavModal`, then this contains the index to the modal
     * in the `modals` array in the context. */
    modalPtr?: number    
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
    /** The Nav always starts with the `MainScreen`. */
    screenGroup: NavGroupType.MainScreen
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
            state.screenGroup = NavGroupType.MainScreen;
            state.stackPtr = undefined;
            state.modalPtr = undefined; 
        },
        injectStack: (state, payload) => {
            let stackIndex: number = payload.payload;

            state.screenGroup = NavGroupType.NavStack;
            state.stackPtr = stackIndex;
        },
        injectModal: (state, payload) => {
            let modalIndex: number = payload.payload;

            state.screenGroup = NavGroupType.NavModal;
            state.stackPtr = modalIndex;
        }
    },    
});

const { openNav, closeNav, toggleNav, injectMainView, injectStack, injectModal } = navSlice.actions;
const useAppDispatch: () => AppDispatch = useDispatch
const useAppSelector: TypedUseSelectorHook<RootState> = useSelector

const store = configureStore({ 
    reducer: {
        nav: navSlice.reducer,
    }, 
});

type RootState = ReturnType<typeof store.getState>;
type AppDispatch = typeof store.dispatch;

/*************************************
 *   Hooks for mutating the state.   *
 *************************************/ 

export const setNavScreen = (type: NavGroupType, id?: string): void => {
    const context = useContext(NavContext);
    const dispatch = useAppDispatch();    
    const setMainView = () => dispatch(injectMainView());
    const setStackView = (index: number) => dispatch(injectStack({payload: index}));
    const setModalView = (index: number) => dispatch(injectModal({payload: index}));

    switch (type)  {   
        case NavGroupType.MainScreen:
            setMainView();

        case NavGroupType.NavModal:
            // We should have an id, so use it to get the index of the modal:
            if (id) {
                // Look up the index for the corresponding screen with identifier id:
                let indexMaybe = findModal(id);
                let index = 0;

                // Make sure we found it:
                switch (indexMaybe.type) {
                    case "nothing":
                        throw new Error (`getNavScreen: id = ${id}, but it was not found by findModal.`)        
                    case "just":
                        index = indexMaybe.value;
                }
                // Set the state to the corresponding modal:
                setModalView(index);                
            } else {
                throw new Error ('getNavScreen: id is undefined, but type was set to NavGroupType.NavModal.')
            }
        case NavGroupType.NavStack:
            // We should have an id, so use it to get the index of the modal:
            if (id) {
                // Look up the index for the corresponding screen with identifier id:
                let indexMaybe = findStack(id);
                let index = 0;

                // Make sure we found it:
                switch (indexMaybe.type) {
                    case "nothing":
                        throw new Error (`getNavScreen: id = ${id}, but it was not found by findStack.`)        
                    case "just":
                        index = indexMaybe.value;
                }
                // Set the state to the corresponding stack:
                setStackView(index);                
            } else {
                throw new Error ('getNavScreen: id is undefined, but type was set to NavGroupType.NavStack.')
            }
    }
}

export const getNavScreen = (): ReactNode => {
    const context = useContext(NavContext);
    const groupType = useAppSelector((state:RootState) => state.nav.screenGroup);
    
    switch (groupType) {
        case NavGroupType.MainScreen: {
            return context.mainScreen.screen;        
        }

        case NavGroupType.NavModal: {
            let indexMaybe: number | undefined = useAppSelector((state:RootState) => state.nav.modalPtr);

            if (indexMaybe) {
                let index: number = indexMaybe;
                return context.modals.group[index].screen;
            } else {
                throw new Error (`getNavScreen: groupType = ${groupType} and modalPtr in the state is undefined.`)
            }
        }
        case NavGroupType.NavStack: {
            let indexMaybe: number | undefined = useAppSelector((state:RootState) => state.nav.stackPtr);

            if (indexMaybe) {
                let index: number = indexMaybe;
                /* Return the first screen in the stack:                
                 * 
                 * TODO: Change this so we can move between screens of the stack:
                 */
                return context.stacks[index].group[0].screen;
            } else {
                throw new Error (`getNavScreen: groupType = ${groupType} and stackPtr in the state is undefined.`)
            }
        }
    }
}

export const isNavOpened = (): boolean => {
    const navStatus = useAppSelector((state:RootState) => state.nav.navStatus);

    return navStatus == navOpened;
}

export const isNavClosed = (): boolean => {    
    return !isNavOpened();
}

export const toggleNavStatus = (): void => {
    const dispatch = useAppDispatch();
    const togNav = () => dispatch(toggleNav());

    togNav();
}

export const openTheNav = (): void => {
    const dispatch = useAppDispatch();
    const navOpen = () => dispatch(openNav());

    navOpen();
}

/** Components for utilizing the store and context. */

export interface NavProviderProps {
    children: ReactNode;
}

/**
 * The Nav Provider.
 */
export default ({children} : NavProviderProps) => {
    return (
        <Provider store={store}>
            <NavContext.Provider value={initialNavContext}>
                {children}
            </NavContext.Provider>
        </Provider>
    )
}