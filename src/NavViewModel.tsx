import { configureStore, createSlice } from "@reduxjs/toolkit";
import { Provider, TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";
import React, { Children, JSXElementConstructor, ReactElement, ReactNode, createContext, useContext } from "react";
import { Button, GestureResponderEvent, View } from "react-native";

import Maybe, { just, nothing } from "./Maybe";
import Bimap from "./Bimap";

interface NavOpened {
    status: 'opened';
}
  
interface NavClosed {
    status: 'closed';
}

type NavStatus = NavOpened | NavClosed;

export const navOpened: NavStatus = {status: 'opened'}
export const navClosed: NavStatus = {status: 'closed'}

/** The state of a Nav screen. */
export interface NavScreenProps {
    /** Some identifier for the screen. */
    id: string,
    /** The screens view. */
    screen: ReactElement
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
    mainScreen: {id: NavGroupType.MainScreen, screen: React.createElement("NavScreen")},
    stacks: [],
    modals: {type: NavGroupType.NavModal, id: '', group: []}
}

/**
 * Finds the index of the corresponding screen group with identifier `id`.
 * 
 * @param id The identifier of the group.
 * @returns The `index` for the corresponding group, or nothing.
 */
const findIndexInGroup = (context: NavContext, group: NavGroupType, id: string) => {
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
const findStack = (context: NavContext, id: string) => {
    return findIndexInGroup(context, NavGroupType.NavStack, id);
}

/**
 * Finds the index of the corresponding modal with identifier `id`.
 * 
 * @param id The identifier of the modal.
 * @returns The `index` for the corresponding modal, or nothing.
 */
const findModal = (context: NavContext, id: string) => {
    return findIndexInGroup(context, NavGroupType.NavModal, id);
}

/** 
 * Sets up the context. */
export const NavContext = createContext(initialNavContext);

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
    if (React.isValidElement(child) && typeof child.type != 'string') {
        // We have to cast child.type, because NavScreen is a function component.   
        let component:JSXElementConstructor<any> = child.type     

        // 1. Make sure that we are in a NavScreen component:
        if (component.name == 'NavScreen') {                    
            // 2. Pattern match over the possible screens:
            switch (child.props.id) {
            case NavGroupType.MainScreen:
                // 3.a. Extract the main screen:
                acc.mainScreen.screen = child.props.screen;
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
export const createNavContext = (children: ReactNode) => {
    // TODO: Change this to just setup the context with this new context:
    return Children.toArray(children).reduce<NavContext>(extractScreens, initialNavContext);
}

interface NavState {
    /** Used to toggle the Nav from opened to closed. */
    navStatus: NavStatus,    

    /** Determines which screen should be shown in the Nav. */
    screenGroup: NavGroupType,
    
    /**
     * Bimap between stack screen id's and array indices into the Context of
     * stack screens. 
     */
    stackScreenMap: Bimap<string,number>,

    /**
     * Bimap between modal screen id's and array indices into the Context of
     * modal screens. 
     */
    modalScreenMap: Bimap<string,number>,

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
    screenGroup: NavGroupType.MainScreen,
    /** The stack screen map starts out empty. */
    stackScreenMap: new Bimap<string,number>(),
    /** The modal screen map starts out empty. */
    modalScreenMap: new Bimap<string,number>()
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
            state.screenGroup = NavGroupType.MainScreen;
            state.stackPtr = undefined;
            state.modalPtr = undefined; 
        },
        /**
         * Injects a stack into the Nav.
         * @param payload The id of the stack.
         */
        injectStack: (state, {payload}) => {
            let stackIndexM: Maybe<number> = state.stackScreenMap.getValue(payload);

            switch(stackIndexM.type) {
                case "just":
                    state.screenGroup = NavGroupType.NavStack;
                    state.stackPtr = stackIndexM.value;
                case "nothing":
                    return
            }            
        },
        /**
         * Injects a modal into the Nav.
         * @param payload The id of the modal.
         */
        injectModal: (state, {payload}) => {
            let modalIndexM: Maybe<number> = state.modalScreenMap.getValue(payload);

            switch(modalIndexM.type) {
                case "just":
                    state.screenGroup = NavGroupType.NavModal;
                    state.stackPtr = modalIndexM.value;
                case "nothing":
                    return
            }
        },
        addStackScreen: (state, {payload}) => {
            state.stackScreenMap.set(payload.key,payload.value);
        },
        addModalScreen: (state, {payload}) => {
            state.modalScreenMap.set(payload.key,payload.value);
        }
    },    
});

export const { openNav, closeNav, toggleNav, injectMainView, injectStack, injectModal } = navSlice.actions;
export const useAppDispatch: () => AppDispatch = useDispatch
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector

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

interface ScreenFound {
    type: NavGroupType,
    index: number
}

export const setNavScreen = (context:NavContext, type: NavGroupType, id?: string): number => {
    // const dispatch = useAppDispatch();    
    // const setMainView = () => dispatch(injectMainView());
    // const setStackView = (index: number) => dispatch(injectStack({payload: index}));
    // const setModalView = (index: number) => dispatch(injectModal({payload: index}));

    switch (type)  {   
        case NavGroupType.MainScreen:
            return 0;

        case NavGroupType.NavModal:
            // We should have an id, so use it to get the index of the modal:
            if (id) {
                // Look up the index for the corresponding screen with identifier id:
                let indexMaybe = findModal(context, id);
                let index = 0;

                // Make sure we found it:
                switch (indexMaybe.type) {
                    case "nothing":
                        throw new Error (`getNavScreen: id = ${id}, but it was not found by findModal.`)        
                    case "just":
                        index = indexMaybe.value;
                }
                // Set the state to the corresponding modal:
                return index;
            } else {
                throw new Error ('getNavScreen: id is undefined, but type was set to NavGroupType.NavModal.')
            }
        case NavGroupType.NavStack:
            // We should have an id, so use it to get the index of the modal:
            if (id) {
                // Look up the index for the corresponding screen with identifier id:
                let indexMaybe = findStack(context, id);
                let index = 0;

                // Make sure we found it:
                switch (indexMaybe.type) {
                    case "nothing":
                        throw new Error (`getNavScreen: id = ${id}, but it was not found by findStack.`)        
                    case "just":
                        index = indexMaybe.value;
                }
                // Set the state to the corresponding stack:
                return index;
            } else {
                throw new Error ('getNavScreen: id is undefined, but type was set to NavGroupType.NavStack.')
            }
    }
}

export const groupTypeSelector = (state:RootState) => {
    return state.nav.screenGroup 
}

export const modalPtrSelector = (state:RootState) => {
    return state.nav.modalPtr
}

export const stackPtrSelector = (state:RootState) => {
    return state.nav.modalPtr
}

export const getNavScreen = (context: NavContext, state: RootState): ReactNode => {            
    const groupType = groupTypeSelector(state);  
    
    switch (groupType) {
        case NavGroupType.MainScreen: {
            return context.mainScreen.screen;        
        }

        case NavGroupType.NavModal: {
            let indexMaybe: number | undefined = modalPtrSelector(state);

            if (indexMaybe) {
                let index: number = indexMaybe;
                return context.modals.group[index].screen;
            } else {
                throw new Error (`getNavScreen: groupType = ${groupType} and modalPtr in the state is undefined.`)
            }
        }
        case NavGroupType.NavStack: {
            let indexMaybe: number | undefined = stackPtrSelector(state);

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

export const NextNavButton = () => {

}

export const GoBackNavButton = () => {

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
