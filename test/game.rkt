#lang c++

(c++

(include "util/bitmap.h"
         "util/trans-bitmap.h"
         "game.h"
         "util/music.h"
         "util/funcs.h"
         "util/font.h"
         "util/pointer.h"
         "util/parameter.h"
         "util/gui/coordinate.h"
         "util/gui/popup-box.h"
         "menu/menu.h"
         "menu/optionfactory.h"
         "menu/menu_option.h"
         "configuration.h"
         "../object/object.h"
         "../object/character.h"
         "../object/player.h"
         "../object/animation.h"
         "../factory/object_factory.h"
         "../factory/heart_factory.h"
         "paintown-engine/level/utils.h"
         "factory/font_render.h"
         "util/token.h"
         "util/tokenreader.h"
         "util/input/keyboard.h"
         "globals.h"
         "../script/script.h"
         "exceptions/shutdown_exception.h"
         "exceptions/exception.h"
         "util/file-system.h"
         "util/system.h"
         "util/events.h"
         "util/loading.h"
         "util/network/network.h"
         "world.h"
         "adventure_world.h"
         "console.h"
         "util/input/input-manager.h"
         "util/input/input-map.h"
         "versus_world.h"
         "util/init.h"
         "util/thread.h"
         <iostream>
         <math.h>
         <string.h>)

(using namespace std)

(template (class Value) vector<Value> Util::Parameter<Value>::stack)

;; static int LAZY_KEY_DELAY = 300;
(static bool show_loading_screen = true)

(namespace Game


(struct Background
    (constructor ()
                 ([string path]
                  [int z = 0])
      ())
    ;; (constructor Background():z(0){}
)

#|
	
static double startingGameSpeed(){
    return 1.0;
}

/*
static void stopLoading(Util::Thread::Id thread){
    if (show_loading_screen){
        Loader::stopLoading(thread);
    }
}

static void startLoading(Util::Thread::Id * thread, const Level::LevelInfo & info ){
    if (show_loading_screen){
        Loader::startLoading(thread, (void*) &info);
    }
}
*/

static Network::Message removeMessage(int id){
    Network::Message message;

    message.id = 0;
    message << World::REMOVE;
    message << id;

    return message;
}

static vector<Background> readBackgrounds( const Filesystem::AbsolutePath & path ){
    vector<Background> backgrounds;

    try{
        TokenReader reader( path.path() + "/bgs.txt" );
        Token * head = reader.readToken();

        if ( *head == "backgrounds" ){
            TokenView view = head->view();
            while (view.hasMore()){
                const Token * background;
                view >> background;
                if ( *background == "background" ){
                    Background b;
                    TokenView backgroundView = background->view();
                    for ( int i = 0; i < 2; i++ ){
                        const Token * next;
                        backgroundView >> next;
                        if ( *next == "path" ){
                            next->view() >> b.path;
                        } else if ( *next == "z" ){
                            next->view() >> b.z;
                        }
                    }
                    backgrounds.push_back(b);
                }
            }
        }

    } catch ( const TokenException & ex ){
        Global::debug( 0 ) << "Could not load " + path.path() + "/bgs.txt because " << ex.getTrace() << endl;
    }

    /*
       Background b1;
       b1.path = path + "/versus/bg1.png";
       b1.z = 420;
       backgrounds.push_back( b1 );
       */

    return backgrounds;
}

static string findNextFile( const char * name ){
    char buf[ 128 ];
    const char * extension = strchr( name, '.' );
    char first[ 128 ];
    strncpy( first, name, extension - name );
    first[ extension - name ] = '\0';
    unsigned int num = 0;
    sprintf( buf, "%s%u%s", first, num, extension );
    do{
        num += 1;
        sprintf( buf, "%s%u%s", first, num, extension );
        /* num != 0 prevents an infinite loop in the extremely
         * remote case that the user has 2^32 files in the directory
         */
    } while (num != 0 && Util::exists(buf));
    return string(buf);
}

static void drawHelp( const Font & font, int x, int y, int color, const Graphics::Bitmap & buffer ){
    font.printf( x, y, color, buffer, "Controls", 0 );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Up: %s", 0,  Keyboard::keyToName( Configuration::config( 0 ).getUp() ) );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Down: %s", 0,  Keyboard::keyToName( Configuration::config( 0 ).getDown() ) );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Left: %s", 0,  Keyboard::keyToName( Configuration::config( 0 ).getLeft() ) );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Right: %s", 0,  Keyboard::keyToName( Configuration::config( 0 ).getRight() ) );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Jump: %s", 0,  Keyboard::keyToName( Configuration::config( 0 ).getJump() ) );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Attack1: %s", 0,  Keyboard::keyToName( Configuration::config( 0 ).getAttack1() ) );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Attack2: %s", 0,  Keyboard::keyToName( Configuration::config( 0 ).getAttack2() ) );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Attack3: %s", 0,  Keyboard::keyToName( Configuration::config( 0 ).getAttack3() ) );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Tab to hide/show minimap", 0 );
    y += font.getHeight() + 1;
    font.printf( x, y, color, buffer, "Press F1 to view this help", 0 );
}

namespace Game{
    enum Input{
        Screenshot,
        Slowdown,
        Speedup,
        NormalSpeed,
        ReloadLevel,
        KillAllHumans,
        Pause,
        MiniMaps,
        ShowHelp,
        ShowFps,
        Quit,
        Console,
    };
}

static void doTakeScreenshot(const Graphics::Bitmap & work){
    string file = findNextFile("paintown-screenshot.bmp");
    Global::debug(2) << "Saved screenshot to " << file << endl;
    work.save(file);
}

/* returns false if players cannot be respawned due to running out of lives */
static bool respawnPlayers(const vector<Paintown::Object*> & players, World & world){
    for ( vector< Paintown::Object * >::const_iterator it = players.begin(); it != players.end(); it++ ){
        Paintown::Character * player = (Paintown::Character *) *it;
        if ( player->getHealth() <= 0 ){
            if ( player->spawnTime() == 0 ){
                player->deathReset();
                if ( player->getLives() == 0 ){
                    return false;
                }
                world.addMessage( removeMessage( player->getId() ) );
                world.addObject( player );
                world.addMessage( player->getCreateMessage() );
                world.addMessage( player->movedMessage() );
                world.addMessage( player->animationMessage() );
            }
        }
    }
    return true;
}

enum MoveListInput{
    Quit,
    Up,
    Down
};

class OptionMoveList: public MenuOption {
public:
    OptionMoveList(const Token * token, Paintown::Player * player):
    MenuOption(token),
    player(player){
        readName(token);
    }

    Paintown::Player * player;

    virtual ~OptionMoveList(){
    }

    virtual void logic(){
    }

    static map<string, Paintown::Animation*> getAttacks(const map<string, Paintown::Animation*> & movements){
        map<string, Paintown::Animation*> out;
        for (map<std::string, Paintown::Animation*>::const_iterator find = movements.begin(); find != movements.end(); find++){
            string name = find->first;
            Paintown::Animation * animation = find->second;
            if (animation->isAttack()){
                out[name] = animation;
            }
        }
        return out;
    }

    void showMoveList(Paintown::Player * player){
        class Logic: public Util::Logic {
        public:
            Logic(Util::ReferenceCount<Paintown::Character> & playerCopy, Gui::PopupBox & area, int & selected):
            playerCopy(playerCopy),
            area(area),
            selected(selected){
                input.set(Keyboard::Key_ESC, 0, false, Quit);
                /* some standard way to set up the keys should be used here */
                input.set(Configuration::config(0).getUp(), 0, true, Up);
                input.set(Configuration::config(0).getDown(), 0, true, Down);
                input.set(Configuration::config(0).getJoystickUp(), 0, true, Up);
                input.set(Configuration::config(0).getJoystickDown(), 0, true, Down);
            }

            Util::ReferenceCount<Paintown::Character> & playerCopy;
            Gui::PopupBox & area;
            InputMap<MoveListInput> input;
            int & selected;

            double ticks(double system){
                return system * Global::LOGIC_MULTIPLIER;
            }

            void run(){
                if (playerCopy->testAnimation()){
                    playerCopy->testReset();
                    playerCopy->setFacing(Paintown::Object::FACING_RIGHT);
                }

                const Font & font = Font::getFont(Global::DEFAULT_FONT, 20, 20);
                area.act(font);

                vector<InputMap<MoveListInput>::InputEvent> events = InputManager::getEvents(input);

                int old = selected;
                for (vector<InputMap<MoveListInput>::InputEvent>::iterator it = events.begin(); it != events.end(); it++){
                    const InputMap<MoveListInput>::InputEvent & event = *it;
                    if (!event.enabled){
                        continue;
                    }

                    if (area.isOpen() && event[Quit]){
                        area.close();
                    }

                    if (event[Up]){
                        selected = selectNext(selected, -1);
                    }

                    if (event[Down]){
                        selected = selectNext(selected, 1);
                    }
                }

                if (old != selected){
                    changeAnimation(selected);
                }
            }

            void changeAnimation(int animation){
                int count = 0;
                const map<string, Paintown::Animation*> movements = getAttacks(playerCopy->getMovements());
                map<std::string, Paintown::Animation*>::const_iterator find;
                for (find = movements.begin(); count != animation && find != movements.end(); find++, count += 1){ /**/ }
                if (find != movements.end()){
                    string name = find->first;
                    playerCopy->testAnimation(name);
                    playerCopy->setFacing(Paintown::Object::FACING_RIGHT);
                }
            }

            int selectNext(int current, int way){
                int size = getAttacks(playerCopy->getMovements()).size();
                int next = current + way;
                if (next < 0){
                    return 0;
                }
                if (next > size - 1){
                    return size - 1;
                }
                return next;
            }

            bool done(){
                return ! area.isActive();
            }
        };

        class Draw: public Util::Draw {
        public:
            Draw(Util::ReferenceCount<Paintown::Character> & playerCopy, Gui::PopupBox & area, int & selected):
                buffer(GFX_X, GFX_Y),
                background(GFX_X, GFX_Y),
                playerCopy(playerCopy),
                area(area),
                selected(selected){
                    background.BlitFromScreen(0, 0);
                    playerCopy->testAnimation("idle");
                }

            Graphics::Bitmap buffer;
            Graphics::Bitmap background;
            Util::ReferenceCount<Paintown::Character> & playerCopy;
            Gui::PopupBox & area;
            int & selected;

            void listMovements(const Graphics::Bitmap & space, int selected){
                int y = 10;
                const Font & font = Font::getFont(Global::DEFAULT_FONT, 20, 20);
                const map<string, Paintown::Animation*> movements = getAttacks(playerCopy->getMovements());
                int count = 0;
                for (map<std::string, Paintown::Animation*>::const_iterator it = movements.begin(); it != movements.end(); it++, count += 1){
                    string name = (*it).first;
                    Paintown::Animation * animation = (*it).second;
                    int color = Graphics::makeColor(255, 255, 255);
                    if (count == selected){
                        color = Graphics::makeColor(27, 237, 239);
                    }
                    font.printf(5, y, color, space, name, 0);
                    y += font.getHeight() + 5;
                }
            }

            void draw(){
                background.Blit(buffer);
                area.render(buffer);
                Graphics::Bitmap space(buffer, area.getArea().getX(), area.getArea().getY(), area.getArea().getWidth() - area.getArea().getRadius(), area.getArea().getHeight() - area.getArea().getRadius());
                // space.clear();
                playerCopy->setX(space.getWidth() / 2 + 50);
                playerCopy->setY(0);
                playerCopy->setZ(space.getHeight() / 2);
                listMovements(space, selected);
                playerCopy->draw(&space, 0, 0);
                // space.border(0, 2, Graphics::makeColor(128, 128, 128));
                buffer.BlitToScreen();
            }
        };

        if (getAttacks(player->getMovements()).size() == 0){
            /* no attacks, failure! */
            Global::debug(0) << "No attacks for " << player->getName() << " so the move list can't be shown" << endl;
            return;
        }

        Util::ReferenceCount<Paintown::Character> playerCopy = new Paintown::Character(*player);
        Gui::PopupBox area;
        area.location.setDimensions(GFX_X - 100, GFX_Y - 100);
        area.location.setCenterPosition(Gui::RelativePoint(0, 0));
        area.location.setRadius(20);

        area.colors.body = Graphics::makeColor(0,0,0);
        area.colors.bodyAlpha = 220;
        area.colors.border = Graphics::makeColor(200,200,200);
        area.colors.borderAlpha = 200;

        area.open();
        int selected = 0;
        Logic logic(playerCopy, area, selected);
        Draw draw(playerCopy, area, selected);

        Util::standardLoop(logic, draw);
    }

    virtual void run(const Menu::Context &){
        showMoveList(player);
    }
};

class GameOptionFactory: public Menu::OptionFactory {
public:
    GameOptionFactory(Paintown::Player * player):
    OptionFactory(),
    player(player){
    }

    Paintown::Player * player;

    MenuOption * getOption(const Token * data) const {
        const Token * head;
        data->view() >> head;
        if (*head == "move-list"){
            return new OptionMoveList(head, player);
        }
        return Menu::OptionFactory::getOption(data);
    }
};

/* in-game menu */
static bool doMenu(const Graphics::Bitmap & screen_buffer, const Token * data, Paintown::Player * player){
    // Menu::Menu menu(Filesystem::find(Filesystem::RelativePath("menu/in-game.txt")));
    GameOptionFactory optionFactory(player);
    Menu::Menu menu(data, optionFactory);
    Menu::Context context;
    /* use the current screen as the background */
    context.addBackground(screen_buffer);
    try{
        menu.run(context);
        /* im pretty sure there is no way to get the menu to return normally,
         * it should always throw an exception either because ESC was pressed
         * or because an option threw something
         */
        return false;
    } catch (const Exception::Quit & forceQuit){
        return true;
    } catch (const Exception::Return & back){
        return false;
    }
}

bool playLevel( World & world, const vector< Paintown::Object * > & players){
    Graphics::Bitmap screen_buffer(GFX_X, GFX_Y);

    /* 150 pixel tall console */
    Console::Console console(150);
    {
        class CommandQuit: public Console::Command{
        public:
            CommandQuit(){
            }

            string act(){
                throw ShutdownException();
            }
        };

        class CommandMemory: public Console::Command {
        public:
            CommandMemory(){
            }

            string act(){
                ostringstream out;
                out << "Memory usage: " << Util::niceSize(System::memoryUsage()) << "\n";
                return out.str();
            }
        };

        class CommandHelp: public Console::Command{
        public:
            CommandHelp(){
            }

            string act(){
                ostringstream out;
                out << "quit - quit the game entirely" << "\n";
                out << "memory - current memory usage" << "\n";
                out << "help - this help menu";
                return out.str();
            }
        };

        console.addCommand("quit", new CommandQuit());
        console.addCommand("help", new CommandHelp());
        console.addCommand("memory", new CommandMemory());
    }
    // bool toggleConsole = false;
    // const int consoleKey = Keyboard::Key_TILDE;

    world.getEngine()->createWorld(world);

    // int game_time = 100;
    // Global::second_counter = 0;

    /* thrown when the player runs out of lives */
    class LoseException: Exception::Base {
    public:
        LoseException():
        Exception::Base(__FILE__, __LINE__){
        }
    };

    /* user presses ESC in the game and brings up the menu. ESC in the menu
     * should go back to the game. selecting the 'exit' option should quit the game
     * to the main menu.
     */ 

    struct GameState{
        GameState():
            menu_quit(false),
            helpTime(0),
            pressed(0),
            done(false),
            show_fps(false),
            takeScreenshot(false){
            }

        bool menu_quit;
        double helpTime;
        int pressed;
        bool done;
        bool show_fps;
        bool takeScreenshot;
    };

    class Logic: public Util::Logic {
    public:
        Logic(const vector<Paintown::Object*> & players, World & world, Console::Console & console, GameState & state, const Graphics::Bitmap & screen_buffer, Token * menuData):
        runCounter(0),
        gameSpeed(startingGameSpeed()),
        players(players),
        helped(false),
        world(world),
        console(console),
        state(state),
        screen_buffer(screen_buffer),
        menuData(menuData){
            if (Global::getDebug() > 0){
                input.set(Keyboard::Key_MINUS_PAD, 2, false, Game::Slowdown);
                input.set(Keyboard::Key_PLUS_PAD, 2, false, Game::Speedup);
                input.set(Keyboard::Key_F4, 0, true, Game::ReloadLevel);
                input.set(Keyboard::Key_F8, 200, false, Game::KillAllHumans);
            }

            input.set(Keyboard::Key_P, 10, false, Game::Pause);
            input.set(Keyboard::Key_TAB, 10, false, Game::MiniMaps);
            input.set(Keyboard::Key_TILDE, 25, false, Game::Console);
            input.set(Keyboard::Key_ESC, 0, false, Game::Quit);
            input.set(Joystick::Quit, 0, false, Game::Quit);
            input.set(Keyboard::Key_F1, 0, false, Game::ShowHelp);
            input.set(Keyboard::Key_F9, 20, false, Game::ShowFps);
            input.set(Keyboard::Key_F12, 10, false, Game::Screenshot);
        }

        InputMap<Game::Input> input;
        double runCounter;
        double gameSpeed;
        const vector<Paintown::Object*> & players;
        bool helped;
        World & world;
        Util::EventManager eventManager;
        Console::Console & console;
        GameState & state;
        const Graphics::Bitmap & screen_buffer;
        Token * menuData;

        virtual bool done(){
            return state.done || state.menu_quit;
        }

        double ticks(double system){
            return system * gameSpeed * Global::LOGIC_MULTIPLIER;
        }

        void doInput(GameState & state, bool & force_quit){
            vector<InputMap<Game::Input>::InputEvent> events = InputManager::getEvents(input);

            bool pressed = false;
            for (vector<InputMap<Game::Input>::InputEvent>::iterator it = events.begin(); it != events.end(); it++){
                const InputMap<Game::Input>::InputEvent & event = *it;
                if (!event.enabled){
                    continue;
                }

                if (event[Game::ShowHelp]){
                    helped = true;
                    state.helpTime = state.helpTime < 260 ? 260 : state.helpTime;
                }

                if (event[Game::ShowFps]){
                    state.show_fps = ! state.show_fps;
                }

                if (event[Game::Console]){
                    console.toggle();
                }

                state.takeScreenshot = event[Game::Screenshot];

                if (event[Game::Pause]){
                    /*
                       paused = ! paused;
                       world.addMessage(paused ? pausedMessage() : unpausedMessage());
                       draw = true;
                     */
                    world.changePause();
                }

                if (event[Game::MiniMaps]){
                    world.drawMiniMaps( ! world.shouldDrawMiniMaps() );
                }

                /*
                   if ( key[ Keyboard::Key_F8 ] ){
                   world.killAllHumans( player );
                   }
                 */

                if ( Global::getDebug() > 0 ){
                    const double SPEED_INC = 0.02;
                    if (event[Game::Speedup]){
                        gameSpeed += SPEED_INC;
                        Global::debug( 3 ) << "Game speed " << gameSpeed << endl;
                    }

                    if (event[Game::Slowdown]){
                        gameSpeed -= SPEED_INC;
                        if ( gameSpeed < SPEED_INC ){
                            gameSpeed = SPEED_INC;
                        }
                        Global::debug( 3 ) << "Game speed " << gameSpeed << endl;
                    }

                    if (event[Game::NormalSpeed]){
                        gameSpeed = 1;
                        Global::debug( 3 ) << "Game speed " << gameSpeed << endl;
                    }

                    if (event[Game::ReloadLevel]){
                        try{
                            world.reloadLevel();
                        } catch ( const LoadException & le ){
                            Global::debug( 0 ) << "Could not reload world: " << le.getTrace() << endl;
                        }
                    }
                }

                force_quit |= event[Game::Quit];
            }

            if (!pressed){
                state.pressed = 0;
            } else {
                state.pressed += 1;
                if (state.pressed > 100){
                    state.pressed = 100;
                    state.helpTime = 260;
                }
            }

            try{
                console.doInput();
            } catch (const Exception::Return & r){
                force_quit = true;
            }
        }

        using Util::Logic::run;
        void run(GameState & state){
            world.act();
            console.act();

            if (!respawnPlayers(players, world)){
                throw LoseException();
            }

            if (state.helpTime > 0){
                if (helped){
                    state.helpTime -= 2;
                } else {
                    state.helpTime -= 0.5;
                }
            }

            bool force_quit = false;
            doInput(state, force_quit);

            state.done |= world.finished();
            if (force_quit){
                Paintown::Player * player = (Paintown::Player*) players[0];
                state.menu_quit = state.menu_quit || doMenu(screen_buffer, menuData, player);
            }
        }

        void run(){
            run(state);
        }

        void waitForQuit(){
            InputManager::waitForRelease(input, Game::Quit);
        }
    };

    class Draw: public Util::Draw {
    public:
        Draw(const Graphics::Bitmap & screen_buffer, Console::Console & console, World & world, GameState & state):
        screen_buffer(screen_buffer),
        console(console),
        world(world),
        state(state),
        /* the game graphics are meant for 320x240 and will be stretched
         * to fit the screen
         */
        work(320, 240),
        frames(0),
        second_counter(Global::second_counter),
        fps(Global::TICS_PER_SECOND){
        }

        const Graphics::Bitmap & screen_buffer;
        Console::Console & console;
        World & world;
        GameState & state;
        Graphics::Bitmap work;
        int frames;
        unsigned int second_counter;
        double fps;

        void updateFrames(){
            if (second_counter != Global::second_counter){
                int difference = Global::second_counter - second_counter;
                double alpha = 0.2;
                /* unlikely, but just in case */
                if (difference == 0){
                    difference = 1;
                }
                fps = (alpha * fps) + ((1 - alpha) * (double) frames / difference);
                // fps[fps_index] = (double) frames / (double) difference;
                // fps_index = (fps_index+1) % max_fps_index;
                second_counter = Global::second_counter;
                frames = 0;
            }

            frames += 1;
        }

        void draw(){
            run(screen_buffer, state);
        }

        void run(const Graphics::Bitmap & screen_buffer, const GameState & state){
            updateFrames();

            world.draw(&work);

            work.Stretch(screen_buffer);
            FontRender * render = FontRender::getInstance();
            render->render(&screen_buffer);

            const Font & font = Font::getFont(Global::DEFAULT_FONT, 20, 20 );

            if (state.helpTime > 0){
                int x = 100;
                int y = screen_buffer.getHeight() / 5;
                int color = Graphics::makeColor( 255, 255, 255 );
                Graphics::Bitmap::transBlender( 0, 0, 0, (int)(state.helpTime > 255 ? 255 : state.helpTime));
                drawHelp( font, x, y, color, screen_buffer.translucent());
            }

            if (state.show_fps){
                font.printf( screen_buffer.getWidth() - 120, 10, Graphics::makeColor(255,255,255), screen_buffer, "FPS: %0.2f", 0, fps );
            }
            console.draw(screen_buffer);

            /* getX/Y move when the world is quaking */
            screen_buffer.BlitToScreen(world.getX(), world.getY());

            if (state.takeScreenshot){
                doTakeScreenshot(work);
            }

            work.clear();
        }

        void showScreenshots(const Graphics::Bitmap & screen_buffer){
            work.clear();
            Sound snapshot(Filesystem::find(Filesystem::RelativePath("sounds/snapshot.wav")).path());
            for (deque<Graphics::Bitmap*>::const_iterator it = world.getScreenshots().begin(); it != world.getScreenshots().end(); it++){
                Graphics::Bitmap * shot = *it;
                int angle = Util::rnd(-6, 6);

                /*
                   int gap = 4;
                   int x = Util::rnd(work.getWidth() - 2 * work.getWidth() / gap) + work.getWidth() / gap;
                   int y = Util::rnd(work.getHeight() - 2 * work.getHeight() / gap) + work.getHeight() / gap;
                   double scale = 1.0 - log(world.getScreenshots().size()+1) / 9.0;
                   shot->greyScale().drawPivot(shot->getWidth() / 2, shot->getHeight() / 2, x, y, angle, scale, work);
                   */

                int x = work.getWidth() / 2;
                int y = work.getHeight() / 2;
                double scale = 0.9;
                shot->border(0, 1, Graphics::makeColor(64,64,64));
                shot->greyScale().drawPivot(shot->getWidth() / 2, shot->getHeight() / 2, x, y, angle, scale, work);
                work.Stretch(screen_buffer);
                screen_buffer.BlitToScreen();
                snapshot.play();
                Util::restSeconds(1.5);
            }
            if (world.getScreenshots().size() > 0){
                Util::restSeconds(2);
            }

        }
    };
    
    TokenReader reader;
    Token * menuData = reader.readToken(Filesystem::find(Filesystem::RelativePath("menu/in-game.txt")).path());

    /* set the current player, mainly so the move list option can work */
    Util::Parameter<Paintown::Player*> currentPlayer((Paintown::Player*) players[0]);

    bool finish = true;
    GameState state;
    Logic logic(players, world, console, state, screen_buffer, menuData);
    Draw drawer(screen_buffer, console, world, state);
    // state.helpTime = helpTime;

    try{
        /* run the game */
        Util::standardLoop(logic, drawer);
        if (!state.menu_quit){
            drawer.showScreenshots(screen_buffer);
        }
    } catch (const LoseException & lose){
        fadeOut(screen_buffer, "You lose");
        finish = false;
    }

#if 0
    try{
        /* Main Loop! */
        while (!state.done && !state.force_quit){
            bool draw = false;
            draw = logic.run(state);

            if (draw){
                drawer.run(screen_buffer, state);
            }

            if (state.force_quit){
                state.force_quit = doMenu(screen_buffer, menuData);
            }

            while (Global::speed_counter < 1){
                logic.rest();
            }
        }

        if (!state.force_quit){
            drawer.showScreenshots(screen_buffer);
        }
    } catch (const LoseException & lose){
        fadeOut(screen_buffer, "You lose");
        finish = false;
    }
#endif

    world.getEngine()->destroyWorld(world);

    if (state.menu_quit){
        logic.waitForQuit();
        finish = false;
    }

    return finish;
}

static string funnyGo(){
    switch (Util::rnd(10)){
        case 0 : return "Robofrance 99";
        case 1 : return "Is that a weasel?";
        case 2 : return "You are the insult master!";
        case 3 : return "Get to the choppaa!!!";
        case 4 : return "Ride or die";
        case 5 : return "Let the Dragon ride on the winds of time";
        case 6 : return "I LIKE TURTLES";
        default : return "Go!";
    }
}

static void initializePlayers(const vector<Paintown::Object*> & players){
    for (vector<Paintown::Object *>::const_iterator it = players.begin(); it != players.end(); it++){
        Paintown::Player * playerX = (Paintown::Player *) *it;
        playerX->setTrails(0, 0);
        playerX->setY(200);
        /* setMoving(false) sets all velocities to 0 */
        playerX->setMoving(false);
        /* but the player is falling so set it back to true */
        playerX->setMoving(true);

        playerX->setStatus(Paintown::Status_Falling);
    }
}

struct GameData{
    GameData(vector<Paintown::Object*> players, const Filesystem::AbsolutePath & path):
        world(players, path),
        players(players){
        }

    AdventureWorld world;
    vector<Paintown::Object*> players;
};

static void realGame(const vector<Util::Future<Paintown::Object*> * > & futurePlayers, const Level::LevelInfo & levelInfo, const string & level){

    class GameContext: public Loader::LoadingContext {
    public:
        GameContext(const vector<Util::Future<Paintown::Object*> * > & futurePlayers, const Filesystem::RelativePath & path):
            data(NULL),
            futurePlayers(futurePlayers),
            path(path),
            failed(NULL){
            }

        virtual ~GameContext(){
            /* who will delete the players contained in the data? the futures
             * passed in as `futurePlayers'
             */
        }

        virtual void failure(){
            if (failed != NULL){
                throw LoadException(*failed);
            }
        }

        virtual void load(){
            try{
                vector<Paintown::Object*> players;
                for (vector<Util::Future<Paintown::Object*>*>::const_iterator fit = futurePlayers.begin(); fit != futurePlayers.end(); fit++){
                    Util::Future<Paintown::Object*> * future = *fit;
                    players.push_back(future->get());
                }
                data = new GameData(players, Filesystem::find(path));
            } catch (const LoadException & exception){
                failed = new LoadException(exception);
            }
        }

        World & getWorld(){
            if (data == NULL){
                throw LoadException(__FILE__, __LINE__, "World didn't load");
            }
            return data->world;
        }

        vector<Paintown::Object*> & getPlayers(){
            if (data == NULL){
                throw LoadException(__FILE__, __LINE__, "Players didn't load");
            }
            return data->players;
        }

        Util::ReferenceCount<GameData> data;
        vector<Util::Future<Paintown::Object*> * > futurePlayers;
        Filesystem::RelativePath path;
        Util::ReferenceCount<LoadException> failed;
    };

    bool gameState = true;
    { /* force scope so the context is destroyed before the factories */
        Global::clearInfo();
        Global::info("Setting up world");
        GameContext context(futurePlayers, Filesystem::RelativePath(level));
        Loader::loadScreen(context, levelInfo);
        context.failure();
        Global::info("World setup");
        Global::info(funnyGo());

        Keyboard::pushRepeatState(false);

        Music::changeSong();

        initializePlayers(context.getPlayers());

        gameState = playLevel(context.getWorld(), context.getPlayers());
    }

    Keyboard::popRepeatState();
    ObjectFactory::destroy();
    HeartFactory::destroy();

    if (! gameState){
        throw Exception::Return(__FILE__, __LINE__);
    }
}

void realGame(const vector<Util::Future<Paintown::Object*> * > & futurePlayers, const Level::LevelInfo & levelInfo){
    for (vector<string>::const_iterator it = levelInfo.getLevels().begin(); it != levelInfo.getLevels().end(); it++){
        realGame(futurePlayers, levelInfo, *it);
    }
}

#if 0
void realGame(const vector<Util::Future<Paintown::Object*> * > & futurePlayers, const Level::LevelInfo & levelInfo){

    /* disables buffer input on entry, enables buffering on function exit */
    /*
    class InputBuffer{
    public:
        InputBuffer(){
            InputManager::disableBufferInput();
        }

        ~InputBuffer(){
            InputManager::enableBufferInput();
        }
    };
    */

    // InputBuffer buffer;

    // Level::LevelInfo levelInfo = Level::readLevels( levelFile );

    // global_debug = true;

    int showHelp = 800;
    for ( vector<string>::const_iterator it = levelInfo.getLevels().begin(); it != levelInfo.getLevels().end(); it++ ){
        Util::Thread::Id loading_screen_thread;
        startLoading(&loading_screen_thread, levelInfo);

        bool gameState = false;
        try {
            // vector< Object * > players;
            // players.push_back( player );

            /*
               Global::debug(0) << "Memory debug loop" << endl;
               for (int i = 0; i < 1000; i++){
               World world( players, *it );
               ObjectFactory::destroy();
               HeartFactory::destroy();
               }
               */

            Global::info("Setting up world");
            vector<Paintown::Object*> players;
            for (vector<Util::Future<Paintown::Object*>*>::const_iterator fit = futurePlayers.begin(); fit != futurePlayers.end(); fit++){
                Util::Future<Paintown::Object*> * future = *fit;
                players.push_back(future->get());
            }

            AdventureWorld world(players, Filesystem::find(Filesystem::RelativePath(*it)));
            Global::info("World setup");
            Global::info(funnyGo());

            Music::pause();
            Music::fadeIn( 0.3 );
            Music::loadSong(Filesystem::getFiles(Filesystem::find(Filesystem::RelativePath("music/")), "*"));
            Music::play();

            for ( vector< Paintown::Object * >::const_iterator it = players.begin(); it != players.end(); it++ ){
                Paintown::Player * playerX = (Paintown::Player *) *it;
                playerX->setTrails(0, 0);
                playerX->setY(200);
                /* setMoving(false) sets all velocities to 0 */
                playerX->setMoving(false);
                /* but the player is falling so set it back to true */
                playerX->setMoving(true);

                playerX->setStatus(Paintown::Status_Falling);
            }

            stopLoading(loading_screen_thread);

            gameState = playLevel(world, players, showHelp);
            showHelp = 0;
        } catch ( const LoadException & le ){
            Global::debug( 0 ) << "Could not load " << *it << " because " << le.getTrace() << endl;
            /* if the level couldn't be loaded turn off
             * the loading screen
             */
            stopLoading( loading_screen_thread );
        } catch (const ShutdownException & se){
            stopLoading(loading_screen_thread);
            throw se;
        }

        ObjectFactory::destroy();
        HeartFactory::destroy();

        if ( ! gameState ){
            return;
        }

        // fadeOut( "Next level" );
    }

    /* fix.. */
    // fadeOut( "You win!" );
}
#endif

#if 0
/* use MenuGlobal::doLevelMenu instead */
const Level::LevelInfo selectLevelSet( const string & base ){
    return MenuGlobals::doLevelMenu(base, NULL);

        Bitmap background( Global::titleScreen() );
	// Bitmap::Screen->Blit( Global::titleScreen() );

	// Bitmap background( Util::getDataPath() + "/paintown-title.png" );
	int fontY = 20;
	const Font & font = Font::getFont( Filesystem::find(DEFAULT_FONT), 20, fontY );
	vector<string> possible = Util::getFiles( base + "/", "*.txt" );
        for ( vector<string>::iterator it = possible.begin(); it != possible.end(); it++ ){
            string & path = *it;
            /*
            path.erase(0, Util::getDataPath().length() + 1);
            */
            path = Filesystem::cleanse(path);
        }

	if ( possible.size() == 0 ){
		return "no-files!!!";
	}

        if (possible.size() == 1){
            return Filesystem::find(possible[0]);
        }

	/*
	for ( vector< string >::iterator it = possible.begin(); it != possible.end(); it++ ){
		string & s = *it;
		s.insert( 0, base + "/" );
	}
	*/
	int choose = 0;

	font.printf( 180, (int)(200 - fontY * 1.2), Bitmap::makeColor( 255, 255, 255 ), background, "Select a set of levels to play", 0 );
	for ( unsigned int i = 0; i < possible.size(); i++ ){
		int yellow = Bitmap::makeColor( 255, 255, 0 );
		int white = Bitmap::makeColor( 255, 255, 255 );
		unsigned int color = i == (unsigned) choose ? yellow : white;
		font.printf( 200, (int)(200 + i * fontY * 1.2), color, background, possible[ i ], 0 );
	}
        background.BlitToScreen();

	Keyboard key;
	bool done = false;

	key.setDelay( Keyboard::Key_UP, LAZY_KEY_DELAY );
	key.setDelay( Keyboard::Key_DOWN, LAZY_KEY_DELAY );
	Global::speed_counter = 0;

	key.wait();

	while ( ! done ){
		
		key.poll();
		bool draw = false;
		if ( Global::speed_counter > 0 ){
			double think = Global::speed_counter;

			while ( think > 0 ){
				think--;

				if ( key[ Keyboard::Key_UP ] ){
					draw = true;
					choose = (choose - 1 + possible.size()) % possible.size();
			}

				if ( key[ Keyboard::Key_DOWN ] ){
					draw = true;
					choose = (choose + 1 + possible.size()) % possible.size();
				}

				if ( key[ Keyboard::Key_ENTER ] ){
					return Filesystem::find(possible[ choose ]);
				}

				if ( key[ Keyboard::Key_ESC ] ){
					throw ReturnException();
				}
			}

			Global::speed_counter = 0;
		}

		if ( draw ){
			for ( unsigned int i = 0; i < possible.size(); i++ ){
				int yellow = Bitmap::makeColor( 255, 255, 0 );
				int white = Bitmap::makeColor( 255, 255, 255 );
				unsigned int color = i == (unsigned) choose ? yellow : white;
				font.printf( 200, (int)(200 + i * fontY * 1.2), color, background, possible[ i ], 0 );
			}
                        background.BlitToScreen();
		}
		
		while ( Global::speed_counter == 0 ){
			Util::rest( 1 );
			key.poll();
		}
	}

	return "nothing-selected";
}
#endif

void fadeOut( const Graphics::Bitmap & work, const string & message ){
    Graphics::Bitmap dark( GFX_X, GFX_Y );
    dark.clear();
    Graphics::Bitmap::transBlender( 0, 0, 0, 128 );

    dark.translucent().draw( 0, 0, work );

    const Font & f = Font::getFont(Global::DEFAULT_FONT, 50, 50 );
    f.printf( 200, 200, Graphics::makeColor( 255, 0, 0 ), work, message, 0 );
    work.BlitToScreen();

    Util::rest( 2000 );
}

static bool closeFloat(double a, double b){
    const double epsilon = 0.0001;
    return fabs(a-b) < epsilon;
}

/* TODO: deprecated */
#if 0
void playVersusMode( Paintown::Character * player1, Paintown::Character * player2, int round ){

	player1->setY( 0 );
	player2->setY( 0 );
	player1->setX( 0 );
	player2->setX( 400 );

	player1->setMaxHealth( 150 );
	player2->setMaxHealth( 150 );
	player1->setHealth( 150 );
	player2->setHealth( 150 );

	// Keyboard key;

	// key.setDelay( Keyboard::Key_P, 100 );

	bool done = false;
	bool paused = false;
	double runCounter = 0;
	double gameSpeed = startingGameSpeed();

	vector< Background > backgrounds;
	backgrounds = readBackgrounds(Filesystem::find(Filesystem::RelativePath("bgs/versus/")));

	Bitmap background( 640, 480 );
	int z = 400;
	if ( backgrounds.size() != 0 ){
		Background use = backgrounds[ Util::rnd( backgrounds.size() ) ];
		Bitmap b(Filesystem::find(Filesystem::RelativePath(use.path)).path());
		b.Stretch(background);
		z = use.z;
	}
	
	VersusWorld world( z, player1, player2 );
	
	Bitmap work( 640, 480 );
	// Bitmap work( GFX_X, GFX_Y );
	Bitmap screen_buffer( GFX_X, GFX_Y );

        Music::changeSong();
        /*
	Music::pause();
	Music::fadeIn( 0.3 );
	Music::loadSong( Util::getFiles( Filesystem::find("/music/"), "*" ) );
	Music::play();
        */

	int roundColors[ 120 ];
	int showRound = sizeof( roundColors ) / sizeof(int) - 1;
	Util::blend_palette( roundColors, 60, Bitmap::makeColor( 96, 0, 0 ), Bitmap::makeColor( 200, 0, 0 ) );
	Util::blend_palette( roundColors + 60, 60, Bitmap::makeColor( 255, 0, 0 ), Bitmap::makeColor( 96, 0, 0 ) );

	while ( ! done ){

		bool draw = false;
		// key.poll();

		if ( Global::speed_counter > 0 ){
			if ( ! paused ){
				runCounter += Global::speed_counter * gameSpeed * Global::LOGIC_MULTIPLIER;

				while ( runCounter >= 1.0 ){
					draw = true;
					world.act();
					runCounter -= 1.0;

					if ( player1->getHealth() <= 0 || player2->getHealth() <= 0 ){
						if ( player1->getHealth() <= 0 && player2->getHealth() > 0 ){
							fadeOut( screen_buffer, "Player 2 wins!" );
						} else if ( player1->getHealth() > 0 && player2->getHealth() <= 0 ){
							fadeOut( screen_buffer, "Player 1 wins!" );
						} else {
							fadeOut( screen_buffer, "Draw!" );
						}
						return;
					}
				}
			}

                        /* FIXME */
                        /*
			const double SPEED_INC = 0.02;
			if ( key[Keyboard::Key_MINUS_PAD] ){
				gameSpeed -= SPEED_INC;
				if ( gameSpeed < SPEED_INC ){
					gameSpeed = SPEED_INC;
				}
				Global::debug( 3 ) << "Game speed " << gameSpeed << endl;
			}

			if ( key[ Keyboard::Key_ESC ] ){
				throw Exception::Return(__FILE__, __LINE__);
			}

			if ( key[ Keyboard::Key_P ] ){
				paused = ! paused;
				draw = true;
			}

			if ( key[ Keyboard::Key_PLUS_PAD ] ){
				gameSpeed += SPEED_INC;
				Global::debug( 3 ) << "Game speed " << gameSpeed << endl;
			}

			if ( key[ Keyboard::Key_ENTER_PAD ] ){
				gameSpeed = 1;
				Global::debug( 3 ) << "Game speed " << gameSpeed << endl;
			}
                        */

			Global::speed_counter = 0;
		} else {
			Util::rest( 1 );
		}
		
		/*
		while ( Global::second_counter > 0 ){
			game_time--;
			Global::second_counter--;
			if ( game_time < 0 )
				game_time = 0;
		}
		*/
	
		if ( draw ){
			const Font & font = Font::getFont(Global::DEFAULT_FONT);

			background.Blit( work );
			world.draw( &work );
			
			/*

			// work.printf( 180, 1, Bitmap::makeColor(255,255,255), (FONT *)all_fonts[ JOHNHANDY_PCX ].dat, "%d", game_time );

			int min_x = (int)(player1->getX() < player2->getX() ? player1->getX() - 50 : player2->getX() - 50);
			int max_x = (int)(player1->getX() > player2->getX() ? player1->getX() + 50 : player2->getX() + 50);
			int min_y = 0;
			// int max_y = screen_buffer.getHeight();

			while ( max_x - min_x < screen_buffer.getWidth() / 2 ){
				max_x += 1;
				min_x -= 1;
			}

			if ( min_x > screen_buffer.getWidth() / 2 ){
				min_x = screen_buffer.getWidth() / 2;
			}
			if ( min_x < 0 ){
				min_x = 0;
			}
			if ( max_x < screen_buffer.getWidth() / 2 ){
				max_x = screen_buffer.getWidth() / 2;
			}
			if ( max_x > screen_buffer.getWidth() ){
				max_x = screen_buffer.getWidth();
			}
	
			/ * split is the number of pixels to show in the Y direction * /
			int split = screen_buffer.getHeight() * (max_x - min_x) / screen_buffer.getWidth();
			/ * cut the difference into two pieces, min_y and max_y * /
			min_y = (screen_buffer.getHeight() - split);
			// max_y -= (screen_buffer.getHeight() - split) / 2;

			// work.Stretch( screen_buffer, min_x, min_y, max_x - min_x, max_y - min_y, 0, 0, screen_buffer.getWidth(), screen_buffer.getHeight() );
			
			*/

			// work.Blit( screen_buffer );
			const double divider = 5;
			const double x_distance = screen_buffer.getWidth() / divider;
			double min_x_1 = player1->getX() - x_distance;
			double max_x_1 = player1->getX() + x_distance;
			double min_x_2 = player2->getX() - x_distance;
			double max_x_2 = player2->getX() + x_distance;

			if ( min_x_1 < 0 ){
				max_x_1 += 0 - min_x_1;
				min_x_1 = 0;
			}
			if ( max_x_1 > screen_buffer.getWidth() ){
				min_x_1 -= max_x_1 - screen_buffer.getWidth();
				max_x_1 = screen_buffer.getWidth();
			}
			
			if ( min_x_2 < 0 ){
				max_x_2 += 0 - min_x_2;
				min_x_2 = 0;
			}
			if ( max_x_2 > screen_buffer.getWidth() ){
				min_x_2 -= max_x_2 - screen_buffer.getWidth();
				max_x_2 = screen_buffer.getWidth();
			}

			if ( (min_x_1 < min_x_2 && max_x_1 > min_x_2) ||
			     (min_x_2 < min_x_1 && max_x_2 > min_x_1) ||
			     (closeFloat(min_x_1, min_x_2)) ){
			     /* the players are close enough together to show
			      * them in the same screen
			      */

				double space = x_distance * 4 - fabs( player1->getX() - player2->getX() );
				double p1 = player1->getX() < player2->getX() ? player1->getX() : player2->getX();
				double p2 = player1->getX() >= player2->getX() ? player1->getX() : player2->getX();
				double x1 = p1 - space / 2;
				double x2 = p2 + space / 2;

				if ( x2 > screen_buffer.getWidth() ){
					x1 -= x2 - screen_buffer.getWidth();
					x2 = screen_buffer.getWidth();
				} else if ( x1 < 0 ){
					x2 += 0 - x1;
					x1 = 0;
				}
				

				/*
				int y1 = (int)(distance / 2 - screen_buffer.getHeight() / divider);
				int y2 = plane + screen_buffer.getHeight();
				*/
				double visible = screen_buffer.getHeight() * (divider - 1) / divider;
				double y1 = world.getMinimumZ() - visible / 2;
				double y2 = world.getMinimumZ() + visible / 2;
				if ( y1 < 0 ){
					y2 += - y1;
					y1 = 0;
				} else if ( y2 > screen_buffer.getHeight() ){
					y1 -= y2 - screen_buffer.getHeight();
					y2 = screen_buffer.getHeight();
				}

				work.Stretch( screen_buffer, (int)x1, (int)y1, (int)(x2 - x1), (int)(y2 - y1), 0, 0, screen_buffer.getWidth(), screen_buffer.getHeight() );

			} else {
				/* split screen */

				/*
				int y1 = (int)(screen_buffer.getHeight() / divider);
				int y2 = screen_buffer.getHeight();
				*/

				double visible = screen_buffer.getHeight() * (divider - 1) / divider;
				double y1 = world.getMinimumZ() - visible / 2;
				double y2 = world.getMinimumZ() + visible / 2;
				if ( y1 < 0 ){
					y2 += - y1;
					y1 = 0;
				} else if ( y2 > screen_buffer.getHeight() ){
					y1 -= y2 - screen_buffer.getHeight();
					y2 = screen_buffer.getHeight();
				}
				int p = player1->getX() < player2->getX() ? 0 : screen_buffer.getWidth() / 2;
				work.Stretch( screen_buffer, (int)min_x_1, (int)y1, (int)(max_x_1 - min_x_1), (int)(y2 - y1), p, 0, screen_buffer.getWidth() / 2, screen_buffer.getHeight() );
				work.Stretch( screen_buffer, (int)min_x_2, (int)y1, (int)(max_x_2 - min_x_2), (int)(y2 - y1), screen_buffer.getWidth() / 2 - p, 0, screen_buffer.getWidth() / 2, screen_buffer.getHeight() );
			}
			     

			if ( showRound > 0 ){
				font.printf( screen_buffer.getWidth() / 2, screen_buffer.getHeight() / 2, roundColors[ showRound ], screen_buffer, "Round %d", 0, round );
				showRound -= 1;
			}

			
			font.printf( 10, 0, Bitmap::makeColor( 255, 255, 255 ), screen_buffer, "%s", 0, player1->getName().c_str() );
			player1->drawLifeBar( 10, font.getHeight(), &screen_buffer );
			font.printf( screen_buffer.getWidth() - 200, 0, Graphics::Bitmap::makeColor( 255, 255, 255 ), screen_buffer, "%s", 0, player2->getName().c_str() );
			player2->drawLifeBar( screen_buffer.getWidth() - 200, font.getHeight(), &screen_buffer );

			FontRender * render = FontRender::getInstance();
			render->render( &screen_buffer );

			if ( paused ){
				screen_buffer.transBlender( 0, 0, 0, 128 );
				screen_buffer.drawingMode( Graphics::Bitmap::MODE_TRANS );
				screen_buffer.rectangleFill( 0, 0, screen_buffer.getWidth(), screen_buffer.getHeight(), Graphics::Bitmap::makeColor( 0, 0, 0 ) );
				screen_buffer.drawingMode( Graphics::Bitmap::MODE_SOLID );
				const Font & font = Font::getFont(Global::DEFAULT_FONT);
				font.printf( screen_buffer.getWidth() / 2, screen_buffer.getHeight() / 2, Graphics::Bitmap::makeColor( 255, 255, 255 ), screen_buffer, "Paused", 0 );
			}

			/* getX/Y move when the world is quaking */
			screen_buffer.BlitToScreen( world.getX(), world.getY() );

                        /*
			if ( key[ Keyboard::Key_F12 ] ){
				Global::debug( 2 ) << "Saved screenshot to scr.bmp" << endl;
				work.save( "scr.bmp" );
			}
                        */

			work.clear();
		}

		// done |= key[ Keyboard::Key_ESC ] || world.finished();
		done = world.finished();
	}
}
#endif

}
|#

)
)
